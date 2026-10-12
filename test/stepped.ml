open! Core
open Exchange_core
open Mm_core

module Step = struct
  type t =
    { time_ms : int
    ; book : L2.t
    ; fills : Fill.t list
    ; plan : Action.t list
    ; strategy : Strategy.t
    }
end

type exchange =
  { random : Random.State.t
  ; mutable resting : Open_order.t list
  ; mutable next_oid : int
  ; mutable next_tid : int
  }

let touch (book : L2.t) side =
  let level =
    match (side : Side.t) with
    | Buy -> L2.best_bid book
    | Sell -> L2.best_ask book
  in
  Option.map level ~f:(fun (l : L2.Level.t) -> l.px)
;;

let fill ex ~now (o : Open_order.t) ~px ~size ~is_maker : Fill.t =
  ex.next_tid <- ex.next_tid + 1;
  { trade_id = Trade_id.of_int ex.next_tid
  ; order_id = o.order_id
  ; client_order_id = o.client_order_id
  ; symbol = o.symbol
  ; side = o.side
  ; px
  ; size
  ; fee = Usd.zero
  ; is_maker
  ; time = now
  ; closed_pnl = None
  }
;;

(* Resting orders the book traded through fill in full; others at or near the touch fill
   by chance. *)
let match_resting ex ~now ~fill_probability (book : L2.t) =
  let fills, resting =
    List.partition_map ex.resting ~f:(fun (o : Open_order.t) ->
      let opposite = touch book (Side.flip o.side) in
      let same = touch book o.side in
      let traded_through =
        Option.exists opposite ~f:(fun px ->
          match o.side with
          | Buy -> Price.(px <= o.px)
          | Sell -> Price.(px >= o.px))
      in
      let bps_away =
        Option.value_map same ~default:Float.infinity ~f:(fun px ->
          Float.abs (Fx.bps_between ~base:px o.px))
      in
      let lucky =
        Float.(
          Random.State.float ex.random 1.
          < fill_probability /. (2. ** Float.round_down bps_away))
      in
      if traded_through || lucky
      then First (fill ex ~now o ~px:o.px ~size:o.size ~is_maker:true)
      else Second o)
  in
  ex.resting <- resting;
  fills
;;

let execute ex ~now (book : L2.t) plan =
  let cancelled =
    List.filter_map plan ~f:(function
      | Action.Cancel oid -> Some oid
      | Keep _ | Place _ -> None)
  in
  ex.resting
  <- List.filter ex.resting ~f:(fun (o : Open_order.t) ->
       not (List.mem cancelled o.order_id ~equal:Order_id.equal));
  List.filter_map plan ~f:(function
    | Action.Keep _ | Cancel _ -> None
    | Place (r : Order_request.t) ->
      ex.next_oid <- ex.next_oid + 1;
      let o : Open_order.t =
        { symbol = r.symbol
        ; side = r.side
        ; px = Option.value_exn r.px
        ; size = r.size
        ; orig_size = Some r.size
        ; order_id = Order_id.of_int ex.next_oid
        ; client_order_id = r.client_order_id
        ; timestamp = now
        }
      in
      let opposite = touch book (Side.flip r.side) in
      let crosses =
        Option.exists opposite ~f:(fun px ->
          match r.side with
          | Buy -> Price.(px <= o.px)
          | Sell -> Price.(px >= o.px))
      in
      (match r.order_type with
       | Limit Alo ->
         if not crosses then ex.resting <- ex.resting @ [ o ];
         None
       | Limit Ioc | Market _ ->
         if crosses
         then
           Some
             (fill ex ~now o ~px:(Option.value_exn opposite) ~size:r.size ~is_maker:false)
         else None
       | Limit Gtc ->
         ex.resting <- ex.resting @ [ o ];
         None))
;;

let run ?(fill_probability = 0.3) ~seed (params : Params.t) script =
  let ex =
    { random = Random.State.make [| seed |]; resting = []; next_oid = 0; next_tid = 0 }
  in
  let strategy = Strategy.create params in
  List.folding_map script ~init:strategy ~f:(fun strategy (time_ms, bid, ask) ->
    let now = time_ms * 1_000_000 in
    let book : L2.t =
      { symbol = params.symbol
      ; time = now
      ; bids = [ { px = Price.of_string bid; sz = Size.one; n = 1 } ]
      ; asks = [ { px = Price.of_string ask; sz = Size.one; n = 1 } ]
      }
    in
    let strategy = Strategy.on_book strategy book in
    let fills = match_resting ex ~now ~fill_probability book in
    let strategy = List.fold fills ~init:strategy ~f:Strategy.on_fill in
    let strategy =
      if List.is_empty fills
      then strategy
      else Strategy.on_open_orders strategy ex.resting
    in
    let strategy, plan = Strategy.tick strategy ~now in
    let ioc_fills = execute ex ~now book plan in
    let strategy = List.fold ioc_fills ~init:strategy ~f:Strategy.on_fill in
    let strategy = Strategy.on_open_orders strategy ex.resting in
    strategy, { Step.time_ms; book; fills = fills @ ioc_fills; plan; strategy })
;;

let print steps =
  List.iter steps ~f:(fun (s : Step.t) ->
    let px side = Option.value_map (touch s.book side) ~default:"-" ~f:Price.to_string in
    let fills =
      List.map s.fills ~f:(fun (f : Fill.t) ->
        sprintf
          "%s %s@%s"
          (if Side.is_buy f.side then "B" else "S")
          (Size.to_string f.size)
          (Price.to_string f.px))
      |> String.concat ~sep:" "
    in
    let count f = List.count s.plan ~f in
    let inventory = Strategy.inventory s.strategy in
    let pnl =
      Option.value_map
        (Market_state.mid (Strategy.market s.strategy))
        ~default:"-"
        ~f:(fun mark ->
          Usd.to_string
            (Usd.round_nearest (Inventory.total_pnl inventory ~mark) ~decimals:3))
    in
    printf
      "%5d %13s | fills %-33s | c%d p%d | pos %-7s pnl %-7s %s\n"
      s.time_ms
      (px Buy ^ "/" ^ px Sell)
      (if String.is_empty fills then "-" else fills)
      (count (function
        | Action.Cancel _ -> true
        | Keep _ | Place _ -> false))
      (count (function
        | Action.Place _ -> true
        | Keep _ | Cancel _ -> false))
      (Size.to_string (Inventory.position inventory))
      pnl
      (Sexp.to_string [%sexp (Strategy.mode s.strategy : Strategy.Mode.t)]))
;;
