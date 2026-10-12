open! Core
open! Async
open Exchange_core
open Mm_core
open Helpers

type config = unit

type t =
  { mutable orders : Open_order.t list
  ; mutable next_oid : int
  ; mutable book_writers : L2.t Pipe.Writer.t list
  ; mutable reconnect_hooks : (unit -> unit) list
  ; fills_r : Fill.t Pipe.Reader.t
  ; fills_w : Fill.t Pipe.Writer.t
  ; updates_r : Order_update.t Pipe.Reader.t
  ; updates_w : Order_update.t Pipe.Writer.t
  ; mutable closed : bool
  ; mutable position : (Size.t * Price.t) option
  }

let connect () =
  let fills_r, fills_w = Pipe.create () in
  let updates_r, updates_w = Pipe.create () in
  return
    (Ok
       { orders = []
       ; next_oid = 0
       ; book_writers = []
       ; reconnect_hooks = []
       ; fills_r
       ; fills_w
       ; updates_r
       ; updates_w
       ; closed = false
       ; position = None
       })
;;

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
  t.book_writers <- List.filter t.book_writers ~f:(fun w -> not (Pipe.is_closed w));
  List.iter t.book_writers ~f:(fun w -> Pipe.write_without_pushback w book)
;;

let push_fill t (fill : Fill.t) =
  if Option.is_none t.position then t.position <- Some (fill.size, fill.px);
  Pipe.write_without_pushback t.fills_w fill
;;

let fills t = t.fills_r
let order_updates t = t.updates_r
let open_orders t _ = return (Ok t.orders)

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
           Option.to_list t.position
           |> List.map ~f:(fun (size, entry_px) : Position_view.t ->
             { symbol = btc
             ; size
             ; entry_px
             ; notional = Fx.notional entry_px size
             ; unrealized_pnl = Usd.zero
             ; realized_pnl = Usd.zero
             ; cum_funding = Usd.zero
             ; liquidation_px = None
             ; leverage = 20
             ; max_leverage = 40
             ; margin_used = Usd.zero
             })
       ; time = 0
       })
;;

let place t requests =
  return
    (Ok
       (List.map requests ~f:(fun (r : Order_request.t) : Order_status.t ->
          match r.px with
          | None -> Rejected (Unsupported "no market orders here")
          | Some px ->
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
                   ; timestamp = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
                   }
                 ];
            Resting { order_id; client_order_id = r.client_order_id })))
;;

let cancel t _symbol oids =
  let gone, kept =
    List.partition_tf t.orders ~f:(fun o ->
      List.mem oids o.order_id ~equal:Order_id.equal)
  in
  t.orders <- kept;
  List.iter gone ~f:(fun (o : Open_order.t) ->
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
      });
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
let is_closed t = t.closed
