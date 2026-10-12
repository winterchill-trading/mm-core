open! Core
open Exchange_core

let flatten_level = 255

let cloid ~symbol ~(side : Side.t) ~level ~generation =
  if level < 0 || level > 255
  then raise_s [%message "Reconciler.cloid: level out of range" (level : int)];
  let name = Symbol.to_string symbol in
  let symbol_hex =
    String.init 8 ~f:(fun i -> if i < String.length name then name.[i] else '\000')
    |> String.concat_map ~f:(fun c -> sprintf "%02x" (Char.to_int c))
  in
  let side_byte =
    match side with
    | Buy -> 0
    | Sell -> 1
  in
  Client_order_id.of_string
    (sprintf
       "0x%s%02x%02x%012x"
       symbol_hex
       side_byte
       level
       (generation land 0xffff_ffff_ffff))
;;

let within_tolerance (p : Params.t) ~current ~desired =
  Float.(abs (Fx.bps_between ~base:desired current) <= p.requote_threshold_bps +. 1e-9)
;;

let order_request (p : Params.t) ~generation (q : Quote.t) =
  Order_request.limit
    ~tif:Alo
    ~client_order_id:(cloid ~symbol:p.symbol ~side:q.side ~level:q.level ~generation)
    p.symbol
    q.side
    ~px:q.px
    ~size:q.sz
;;

let reconcile (p : Params.t) ~now ~generation ~open_orders ~desired =
  let ours =
    List.filter open_orders ~f:(fun (o : Open_order.t) -> Symbol.equal o.symbol p.symbol)
  in
  let keepable (o : Open_order.t) = now - o.timestamp < p.order_ttl_ms * 1_000_000 in
  let matches (q : Quote.t) (o : Open_order.t) =
    Side.equal o.side q.side
    && Size.equal o.size q.sz
    && keepable o
    && within_tolerance p ~current:o.px ~desired:q.px
  in
  (* Prices a side's ladder must stay strictly beyond, level after level. *)
  let beyond (side : Side.t) a b =
    match side with
    | Buy -> Price.(a < b)
    | Sell -> Price.(a > b)
  in
  (* The final price of the previous level, per side (buy, sell). *)
  let last_on (side : Side.t) (buy, sell) =
    match side with
    | Buy -> buy
    | Sell -> sell
  in
  let set_last (side : Side.t) (buy, sell) px =
    match side with
    | Buy -> Some px, sell
    | Sell -> buy, Some px
  in
  let next_desired =
    (* For each desired quote, the price of the next desired quote on its side. *)
    let rec go = function
      | [] -> []
      | (q : Quote.t) :: rest ->
        let next =
          List.find rest ~f:(fun (r : Quote.t) -> Side.equal r.side q.side)
          |> Option.map ~f:(fun (r : Quote.t) -> r.px)
        in
        (q, next) :: go rest
    in
    go desired
  in
  let (used, _last), assignments =
    List.fold_map
      next_desired
      ~init:(Order_id.Set.empty, (None, None))
      ~f:(fun (used, last) (q, next) ->
        let in_place (o : Open_order.t) =
          Option.for_all (last_on q.side last) ~f:(fun prev -> beyond q.side o.px prev)
          && Option.for_all next ~f:(fun next -> beyond q.side next o.px)
        in
        let best =
          List.filter ours ~f:(fun o ->
            (not (Set.mem used o.order_id)) && matches q o && in_place o)
          |> List.min_elt ~compare:(fun (a : Open_order.t) (b : Open_order.t) ->
            let dist (o : Open_order.t) =
              abs (Price.to_units o.px - Price.to_units q.px)
            in
            match Int.compare (dist a) (dist b) with
            | 0 -> Order_id.compare a.order_id b.order_id
            | c -> c)
        in
        match best with
        | Some o -> (Set.add used o.order_id, set_last q.side last o.px), `Keep o.order_id
        | None -> (used, set_last q.side last q.px), `Place q)
  in
  let cancels =
    List.filter_map ours ~f:(fun o ->
      Option.some_if (not (Set.mem used o.order_id)) (Action.Cancel o.order_id))
  in
  let places =
    List.filter_map assignments ~f:(function
      | `Place q -> Some (Action.Place (order_request p ~generation q))
      | `Keep _ -> None)
  in
  let keeps =
    List.filter_map assignments ~f:(function
      | `Keep oid -> Some (Action.Keep oid)
      | `Place _ -> None)
  in
  cancels @ places @ keeps
;;
