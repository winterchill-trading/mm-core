open! Core
open! Async
open Exchange_core
open Mm_core
open Helpers

(* An in-memory venue: rests every limit order, cancels on request, and lets the test push
   books and fills. *)
module Memory_venue = struct
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
      (Ok
         (Instrument.create
            symbol
            ~size_decimals:5
            ~price_tick:Price.one
            ~max_leverage:40))
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
end

module Live_memory = Live.Make (Memory_venue)

let rec wait_until ?(timeout = Time_ns.Span.of_int_sec 10) ~what f =
  if f ()
  then return ()
  else if Time_ns.Span.(timeout <= zero)
  then raise_s [%message "timed out waiting" what]
  else (
    let step = Time_ns.Span.of_int_ms 10 in
    let%bind () = Clock_ns.after step in
    wait_until ~timeout:Time_ns.Span.(timeout - step) ~what f)
;;

let live_book () =
  { (book ~bid:"99990" ~ask:"100010" ()) with
    time = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
  }
;;

let%expect_test "the live loop quotes, applies fills and cancels on stop" =
  Log.Global.set_output [];
  let%bind venue = Memory_venue.connect () >>| ok_exn in
  let%bind instrument = Memory_venue.instrument venue btc >>| ok_exn in
  let mid =
    don't_wait_for
      (let%map () = Clock_ns.after (Time_ns.Span.of_int_ms 20) in
       Memory_venue.push_book venue (live_book ()));
    Live_memory.first_mid venue btc
  in
  let%bind mid = mid >>| ok_exn in
  let params =
    { (Params.for_instrument instrument ~mid) with requote_interval_ms = 100 }
  in
  print_s
    [%message (mid : Price.t) (params.order_size : Size.t) (params.max_position : Size.t)];
  [%expect {| ((mid 100000) (params.order_size 0.001) (params.max_position 0.01)) |}];
  let%bind live =
    Live_memory.start venue { params; summary_interval = Time_ns.Span.of_int_sec 60 }
    >>| ok_exn
  in
  Memory_venue.push_book venue (live_book ());
  let%bind () =
    wait_until ~what:"quotes" (fun () -> (Live_memory.report live).orders_placed >= 6)
  in
  List.sort venue.orders ~compare:(fun (a : Open_order.t) b -> Price.descending a.px b.px)
  |> List.iter ~f:(fun (o : Open_order.t) ->
    printf
      "%s %s@%s\n"
      (Sexp.to_string [%sexp (o.side : Side.t)])
      (Size.to_string o.size)
      (Price.to_string o.px));
  [%expect
    {|
    Sell 0.001@100070
    Sell 0.001@100050
    Sell 0.001@100030
    Buy 0.001@99970
    Buy 0.001@99950
    Buy 0.001@99930
    |}];
  (* The same fill twice: applied once. *)
  let fill =
    { (fill ~tid:1 ~side:Buy ~px:"99970" ~sz:"0.001" ~fee:"0.01" ()) with
      time = Time_ns.to_int_ns_since_epoch (Time_ns.now ())
    }
  in
  Memory_venue.push_fill venue fill;
  Memory_venue.push_fill venue fill;
  let%bind () =
    wait_until ~what:"fill" (fun () -> (Live_memory.report live).fills >= 1)
  in
  let%bind report = Live_memory.stop live in
  print_s
    [%message
      ""
        ~fills:(report.fills : int)
        ~maker_fills:(report.maker_fills : int)
        ~position:(report.position : Size.t)
        ~volume:(report.volume : Usd.t)
        ~fees:(report.fees : Usd.t)
        ~account_value:(report.account_value : Usd.t option)
        ~cancelled_at_least_6:(report.orders_cancelled >= 6 : bool)
        ~mode:(report.mode : Strategy.Mode.t)];
  [%expect
    {|
    ((fills 1) (maker_fills 1) (position 0.001) (volume 99.97) (fees 0.01)
     (account_value (1000)) (cancelled_at_least_6 true) (mode Running))
    |}];
  print_s [%message (venue.orders : Open_order.t list) (venue.closed : bool)];
  [%expect {| ((venue.orders ()) (venue.closed true)) |}];
  return ()
;;
