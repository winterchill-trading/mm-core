open! Core
open! Async
open Exchange_core
open Mm_core
open Helpers

module Bugs = struct
  type t =
    { seed : int
    ; reject_percent : int
    ; cancel_ack_delay : Time_ns.Span.t
    ; split_fills : int
    ; stale_poll_percent : int
    ; late_places : int
    ; late_place_delay : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let none =
    { seed = 0
    ; reject_percent = 0
    ; cancel_ack_delay = Time_ns.Span.zero
    ; split_fills = 1
    ; stale_poll_percent = 0
    ; late_places = 0
    ; late_place_delay = Time_ns.Span.zero
    }
  ;;
end

module Injected = struct
  type t =
    { rejects : int
    ; late_cancels : int
    ; split_fills : int
    ; stale_polls : int
    ; late_places : int
    }
  [@@deriving sexp_of]
end

type config = Bugs.t

type t =
  { bugs : Bugs.t
  ; time_source : Time_source.t
  ; random : Random.State.t
  ; mutable injected : Injected.t
  ; mutable orders : Open_order.t list
  ; mutable last_snapshot : Open_order.t list
  ; mutable next_oid : int
  ; mutable places : int
  ; mutable book_writers : L2.t Pipe.Writer.t list
  ; mutable last_book : L2.t option
  ; mutable reconnect_hooks : (unit -> unit) list
  ; fills_r : Fill.t Pipe.Reader.t
  ; fills_w : Fill.t Pipe.Writer.t
  ; updates_r : Order_update.t Pipe.Reader.t
  ; updates_w : Order_update.t Pipe.Writer.t
  ; seen_trades : Trade_id.Hash_set.t
  ; mutable closed : bool
  ; mutable position : Size.t
  ; mutable entry : Price.t option
  }

let create ?(time_source = Time_source.wall_clock ()) (bugs : Bugs.t) =
  let fills_r, fills_w = Pipe.create () in
  let updates_r, updates_w = Pipe.create () in
  { bugs
  ; time_source
  ; random = Random.State.make [| bugs.seed |]
  ; injected =
      { rejects = 0; late_cancels = 0; split_fills = 0; stale_polls = 0; late_places = 0 }
  ; orders = []
  ; last_snapshot = []
  ; next_oid = 0
  ; places = 0
  ; book_writers = []
  ; last_book = None
  ; reconnect_hooks = []
  ; fills_r
  ; fills_w
  ; updates_r
  ; updates_w
  ; seen_trades = Trade_id.Hash_set.create ()
  ; closed = false
  ; position = Size.zero
  ; entry = None
  }
;;

let connect bugs = return (Ok (create bugs))
let now_ns t = Time_ns.to_int_ns_since_epoch (Time_source.now t.time_source)
let percent t p = p > 0 && Random.State.int t.random 100 < p

let instrument _ symbol =
  return
    (Ok (Instrument.create symbol ~size_decimals:5 ~price_tick:Price.one ~max_leverage:40))
;;

let books t _symbol =
  let r, w = Pipe.create () in
  t.book_writers <- w :: t.book_writers;
  return r
;;

let push_book t book =
  t.last_book <- Some book;
  t.book_writers <- List.filter t.book_writers ~f:(fun w -> not (Pipe.is_closed w));
  List.iter t.book_writers ~f:(fun w -> Pipe.write_without_pushback w book)
;;

let deliver_fill t (fill : Fill.t) =
  if not (Hash_set.mem t.seen_trades fill.trade_id)
  then (
    Hash_set.add t.seen_trades fill.trade_id;
    if Option.is_none t.entry then t.entry <- Some fill.px;
    t.position <- Size.(t.position + Fx.signed fill.side fill.size));
  if not (Pipe.is_closed t.fills_w) then Pipe.write_without_pushback t.fills_w fill
;;

(* Cuts [size] into [n] positive lot multiples (fewer if it is too small). *)
let split_size t size ~n =
  let lot = Size.to_units (Size.of_string "0.00001") in
  let lots = Size.to_units size / lot in
  let n = Int.max 1 (Int.min n lots) in
  let cuts =
    List.init (n - 1) ~f:(fun _ -> 1 + Random.State.int t.random (lots - 1))
    |> List.dedup_and_sort ~compare:Int.compare
  in
  let bounds = (0 :: cuts) @ [ lots ] in
  List.zip_exn (List.drop_last_exn bounds) (List.tl_exn bounds)
  |> List.map ~f:(fun (a, b) -> Size.of_units ((b - a) * lot))
;;

let push_fill t (fill : Fill.t) =
  let n =
    if t.bugs.split_fills > 1 then 1 + Random.State.int t.random t.bugs.split_fills else 1
  in
  match split_size t fill.size ~n with
  | [ _ ] | [] -> deliver_fill t fill
  | parts ->
    t.injected <- { t.injected with split_fills = t.injected.split_fills + 1 };
    let total = Size.to_float fill.size in
    List.iteri parts ~f:(fun i size ->
      deliver_fill
        t
        { fill with
          trade_id = Trade_id.of_int ((Trade_id.to_int fill.trade_id * 100) + i)
        ; size
        ; fee = Usd.of_float (Usd.to_float fill.fee *. Size.to_float size /. total)
        })
;;

let fills t = t.fills_r
let order_updates t = t.updates_r

let open_orders t _ =
  if percent t t.bugs.stale_poll_percent
  then (
    t.injected <- { t.injected with stale_polls = t.injected.stale_polls + 1 };
    return (Ok t.last_snapshot))
  else (
    t.last_snapshot <- t.orders;
    return (Ok t.orders))
;;

let account t =
  return
    (Ok
       { Account_view.account = Account.of_string "mm"
       ; balance = usd "1000"
       ; equity = usd "1000"
       ; total_notional = Usd.zero
       ; margin_used = Usd.zero
       ; maintenance_margin = Usd.zero
       ; available = usd "1000"
       ; positions =
           (match t.entry with
            | Some entry_px when not (Size.is_zero t.position) ->
              [ { Position_view.symbol = btc
                ; size = t.position
                ; entry_px
                ; notional = Fx.notional entry_px (Size.abs t.position)
                ; unrealized_pnl = Usd.zero
                ; realized_pnl = Usd.zero
                ; cum_funding = Usd.zero
                ; liquidation_px = None
                ; leverage = 20
                ; max_leverage = 40
                ; margin_used = Usd.zero
                }
              ]
            | _ -> [])
       ; time = 0
       })
;;

let rest t (r : Order_request.t) ~px =
  t.next_oid <- t.next_oid + 1;
  let order_id = Order_id.of_int t.next_oid in
  t.orders
  <- t.orders
     @ [ { symbol = r.symbol
         ; side = r.side
         ; px
         ; size = r.size
         ; orig_size = Some r.size
         ; order_id
         ; client_order_id = r.client_order_id
         ; timestamp = now_ns t
         }
       ];
  order_id
;;

(* A post-only order that would take liquidity from the last pushed book. *)
let would_cross t (r : Order_request.t) ~px =
  match r.order_type, t.last_book with
  | Limit Alo, Some book ->
    (match r.side with
     | Buy ->
       Option.exists (L2.best_ask book) ~f:(fun (l : L2.Level.t) -> Price.(px >= l.px))
     | Sell ->
       Option.exists (L2.best_bid book) ~f:(fun (l : L2.Level.t) -> Price.(px <= l.px)))
  | (Limit (Gtc | Ioc) | Market _), _ | Limit Alo, None -> false
;;

let place t requests =
  t.places <- t.places + 1;
  if t.places <= t.bugs.late_places
  then (
    t.injected <- { t.injected with late_places = t.injected.late_places + 1 };
    upon (Time_source.after t.time_source t.bugs.late_place_delay) (fun () ->
      List.iter requests ~f:(fun (r : Order_request.t) ->
        Option.iter r.px ~f:(fun px -> ignore (rest t r ~px : Order_id.t))));
    return (Or_error.error_string "place: timed out"))
  else
    return
      (Ok
         (List.map requests ~f:(fun (r : Order_request.t) : Order_status.t ->
            match r.px with
            | None -> Rejected (Unsupported "no market orders here")
            | Some px when would_cross t r ~px ->
              let best f =
                Option.bind t.last_book ~f:(fun b ->
                  Option.map (f b) ~f:(fun (l : L2.Level.t) -> l.px))
              in
              Rejected
                (Post_only_would_cross
                   { best_bid = best L2.best_bid; best_ask = best L2.best_ask })
            | Some _ when percent t t.bugs.reject_percent ->
              t.injected <- { t.injected with rejects = t.injected.rejects + 1 };
              Rejected Insufficient_margin
            | Some px ->
              let order_id = rest t r ~px in
              Resting { order_id; client_order_id = r.client_order_id })))
;;

let remove t oids =
  let gone, kept =
    List.partition_tf t.orders ~f:(fun o ->
      List.mem oids o.order_id ~equal:Order_id.equal)
  in
  t.orders <- kept;
  List.iter gone ~f:(fun (o : Open_order.t) ->
    if not (Pipe.is_closed t.updates_w)
    then
      Pipe.write_without_pushback
        t.updates_w
        { order_id = o.order_id
        ; client_order_id = o.client_order_id
        ; symbol = o.symbol
        ; side = o.side
        ; px = o.px
        ; status = Cancelled
        ; filled = Size.zero
        ; remaining = Size.zero
        ; time = 0
        })
;;

let fill_resting t (o : Open_order.t) ~tid =
  let now = now_ns t in
  t.orders
  <- List.filter t.orders ~f:(fun (r : Open_order.t) ->
       not (Order_id.equal r.order_id o.order_id));
  if not (Pipe.is_closed t.updates_w)
  then
    Pipe.write_without_pushback
      t.updates_w
      { order_id = o.order_id
      ; client_order_id = o.client_order_id
      ; symbol = o.symbol
      ; side = o.side
      ; px = o.px
      ; status = Filled
      ; filled = o.size
      ; remaining = Size.zero
      ; time = now
      };
  push_fill
    t
    { trade_id = Trade_id.of_int tid
    ; order_id = o.order_id
    ; client_order_id = o.client_order_id
    ; symbol = o.symbol
    ; side = o.side
    ; px = o.px
    ; size = o.size
    ; fee = Usd.zero
    ; is_maker = true
    ; time = now
    ; closed_pnl = None
    }
;;

let cancel t _symbol oids =
  if Time_ns.Span.(t.bugs.cancel_ack_delay > zero)
  then (
    t.injected <- { t.injected with late_cancels = t.injected.late_cancels + 1 };
    upon (Time_source.after t.time_source t.bugs.cancel_ack_delay) (fun () ->
      remove t oids))
  else remove t oids;
  return (Ok ())
;;

let cancel_all t symbol = cancel t symbol (List.map t.orders ~f:(fun o -> o.order_id))
let on_reconnect t ~f = t.reconnect_hooks <- f :: t.reconnect_hooks

let close t =
  t.closed <- true;
  Pipe.close t.fills_w;
  Pipe.close t.updates_w;
  List.iter t.book_writers ~f:Pipe.close;
  return ()
;;

let orders t = t.orders
let position t = t.position
let injected t = t.injected
let is_closed t = t.closed
