open! Core
open! Async
open Exchange_core
open Mm_core
module Md = Market_data

module Tick = struct
  type t =
    { time : Time_ns.t
    ; bid : Price.t option
    ; ask : Price.t option
    ; mode : Strategy.Mode.t
    ; plan : Action.t list
    }
  [@@deriving sexp_of]

  let to_string_hum t =
    let px = Option.value_map ~default:"-" ~f:Price.to_string in
    let header =
      sprintf
        "%s bid %s ask %s %s"
        (Time_ns.to_string_iso8601_basic t.time ~zone:Time_float.Zone.utc)
        (px t.bid)
        (px t.ask)
        (Sexp.to_string [%sexp (t.mode : Strategy.Mode.t)])
    in
    let changes =
      List.filter_map t.plan ~f:(function
        | Keep _ -> None
        | Cancel oid -> Some (sprintf "  cancel %s" (Order_id.to_string oid))
        | Place (r : Order_request.t) ->
          Some
            (sprintf
               "  place %s %s @ %s%s"
               (String.lowercase (Sexp.to_string [%sexp (r.side : Side.t)]))
               (Size.to_string r.size)
               (px r.px)
               (if r.reduce_only then " reduce-only" else "")))
    in
    let kept =
      List.count t.plan ~f:(function
        | Keep _ -> true
        | _ -> false)
    in
    String.concat
      ~sep:"\n"
      ((header :: changes) @ if kept > 0 then [ sprintf "  keep %d" kept ] else [])
  ;;
end

(* The exchange of a replay: plans are accepted as sent. *)
module Book_keeper = struct
  type t =
    { mutable resting : Open_order.t list
    ; mutable next_oid : int
    }

  let create () = { resting = []; next_oid = 1 }

  let apply t plan ~now =
    List.iter plan ~f:(fun (action : Action.t) ->
      match action with
      | Keep _ -> ()
      | Cancel oid ->
        t.resting
        <- List.filter t.resting ~f:(fun (o : Open_order.t) ->
             not (Order_id.equal o.order_id oid))
      | Place r ->
        (match r.px with
         | None -> ()
         | Some px ->
           let order_id = Order_id.of_int t.next_oid in
           t.next_oid <- t.next_oid + 1;
           t.resting
           <- { Open_order.symbol = r.symbol
              ; side = r.side
              ; px
              ; size = r.size
              ; orig_size = Some r.size
              ; order_id
              ; client_order_id = r.client_order_id
              ; timestamp = now
              }
              :: t.resting));
    t.resting
  ;;
end

let to_l2 ~depth ~symbol ~time book : L2.t =
  let level (l : Md.L2_update.Level.t) = { L2.Level.px = l.price; sz = l.size; n = 1 } in
  { symbol
  ; time
  ; bids = List.map (Md.L2_book.bids ~depth book) ~f:level
  ; asks = List.map (Md.L2_book.asks ~depth book) ~f:level
  }
;;

let best levels =
  List.hd levels |> Option.map ~f:(fun (l : Md.L2_update.Level.t) -> l.price)
;;

let run ?(depth = 20) (feed : Md.Feed.t) ~params ~f =
  let module F = (val feed) in
  Pipe.close_read F.trades;
  Pipe.close_read F.bbo;
  Pipe.close_read F.funding;
  let book = ref Md.L2_book.empty in
  let strategy = ref None in
  let error = ref None in
  let keeper = Book_keeper.create () in
  let step (update : Md.L2_update.t) =
    book := Md.L2_book.apply !book update;
    let bid = best (Md.L2_book.bids ~depth:1 !book) in
    let ask = best (Md.L2_book.asks ~depth:1 !book) in
    match bid, ask with
    | Some bid_px, Some ask_px when Md.L2_book.has_snapshot !book ->
      let created =
        match !strategy with
        | Some s -> Ok s
        | None ->
          let mid =
            Price.of_units ((Price.to_units bid_px + Price.to_units ask_px) / 2)
          in
          Or_error.bind (params ~mid) ~f:(fun p ->
            Or_error.try_with (fun () -> Strategy.create p))
      in
      (match created with
       | Error e -> error := Some e
       | Ok s ->
         let now = Time_ns.to_int_ns_since_epoch update.recv_ts in
         let s =
           Strategy.on_book
             s
             (to_l2 ~depth ~symbol:(Strategy.params s).symbol ~time:now !book)
         in
         let s, plan = Strategy.tick s ~now in
         let s =
           if List.is_empty plan
           then s
           else Strategy.on_open_orders s (Book_keeper.apply keeper plan ~now)
         in
         strategy := Some s;
         f { Tick.time = update.recv_ts; bid; ask; mode = Strategy.mode s; plan })
    | _ -> ()
  in
  let%bind () =
    Pipe.iter_without_pushback F.l2 ~f:(fun update ->
      if Option.is_none !error then step update)
  in
  let%map () = F.close () in
  match !error with
  | Some e -> Error e
  | None -> Ok !strategy
;;

let size_decimals lot =
  match String.lsplit2 (Size.to_string lot) ~on:'.' with
  | None -> 0
  | Some (_, fraction) -> String.length fraction
;;

let catalogue_params (instrument : Md.Instrument.t) ~mid =
  match Md.Instrument_info.lookup instrument with
  | None ->
    Or_error.error_s
      [%message
        "instrument not in the catalogue; pass -params"
          ~instrument:(Md.Instrument.to_string instrument)]
  | Some info ->
    let exchange_instrument =
      Instrument.create
        instrument.symbol
        ~size_decimals:(size_decimals info.lot)
        ~price_tick:info.tick
        ~max_leverage:1
    in
    if not (Size.equal (Instrument.lot_size exchange_instrument) info.lot)
    then
      Or_error.error_s
        [%message "lot is not a power of ten; pass -params" ~lot:(info.lot : Size.t)]
    else Params.validate (Params.for_instrument exchange_instrument ~mid)
;;

let l2_files ~dir ~symbol =
  let store = Md.Store.create dir in
  let%bind instruments = Md.Store.instruments store in
  let matches =
    List.filter instruments ~f:(fun (i : Md.Instrument.t) ->
      String.equal (Md.Instrument.to_string i) symbol
      || String.equal (Md.Symbol.to_string i.symbol) symbol)
  in
  match matches with
  | [] ->
    return
      (Or_error.error_s
         [%message "no recorded instrument" (symbol : string) (dir : string)])
  | _ :: _ :: _ ->
    return
      (Or_error.error_s
         [%message
           "symbol on several venues; use venue:SYMBOL"
             ~instruments:(List.map matches ~f:Md.Instrument.to_string : string list)])
  | [ instrument ] ->
    (match%bind Md.Store.days store Norm instrument L2 with
     | [] ->
       return
         (Or_error.error_s
            [%message
              "no norm/ L2 files" ~instrument:(Md.Instrument.to_string instrument)])
     | first :: _ as days ->
       let%map files =
         Md.Store.files store Norm instrument [ L2 ] ~from:first ~to_:(List.last_exn days)
       in
       Ok (instrument, files))
;;
