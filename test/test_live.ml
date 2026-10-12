open! Core
open! Async
open Exchange_core
open Mm_core
open Helpers
open Live_helpers
module Live_memory = Live.Make (Memory_venue)

let%expect_test "the live loop quotes, applies fills and cancels on stop" =
  Log.Global.set_output [];
  let%bind venue = Memory_venue.connect Memory_venue.Bugs.none >>| ok_exn in
  let%bind instrument = Memory_venue.instrument venue btc >>| ok_exn in
  let mid =
    don't_wait_for
      (let%map () = Clock_ns.after (Time_ns.Span.of_int_ms 20) in
       Memory_venue.push_book venue (live_book ()));
    Live_memory.first_mid venue btc
  in
  let%bind mid = mid >>| ok_exn in
  let params =
    { (Params.for_instrument instrument ~mid) with
      requote_interval_ms = Time_ns.Span.of_int_ms 100
    }
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
  List.sort (Memory_venue.orders venue) ~compare:(fun (a : Open_order.t) b ->
    Price.descending a.px b.px)
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
  print_s
    [%message
      ""
        ~orders:(Memory_venue.orders venue : Open_order.t list)
        ~closed:(Memory_venue.is_closed venue : bool)];
  [%expect {| ((orders ()) (closed true)) |}];
  return ()
;;
