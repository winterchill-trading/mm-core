open! Core
open! Async
open Exchange_core
open Mm_core
open Live_helpers
module Live_memory = Live.Make (Memory_venue)

let params =
  { Params.btc with
    requote_interval_ms = Time_ns.Span.of_int_ms 100
  ; max_data_age_ms = Time_ns.Span.of_int_ms 60_000
  }
;;

let config = { Live.Config.params; summary_interval = Time_ns.Span.of_int_sec 60 }

let start bugs =
  Log.Global.set_output [];
  let%bind venue = Memory_venue.connect bugs >>| ok_exn in
  let%bind live = Live_memory.start venue config >>| ok_exn in
  Memory_venue.push_book venue (live_book ());
  return (venue, live)
;;

let%expect_test "a place that timed out but rested later is adopted, not duplicated" =
  let%bind venue, live =
    start
      { Memory_venue.Bugs.none with
        late_places = 1
      ; late_place_delay = Time_ns.Span.of_int_ms 30
      }
  in
  let%bind () =
    wait_until ~what:"late orders" (fun () -> List.length (Memory_venue.orders venue) = 6)
  in
  (* Two more requote intervals: the snapshot now holds the late orders. *)
  let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 300) in
  print_orders (Memory_venue.orders venue);
  [%expect
    {|
    Sell 0.001@100070 gen 0
    Sell 0.001@100050 gen 0
    Sell 0.001@100030 gen 0
    Buy 0.001@99970 gen 0
    Buy 0.001@99950 gen 0
    Buy 0.001@99930 gen 0
    |}];
  let report = Live_memory.report live in
  print_s
    [%message
      ""
        ~plans:(report.plans : int)
        ~placed:(report.orders_placed : int)
        ~strategy_sees:
          (List.length (Strategy.open_orders (Live_memory.strategy live)) : int)
        ~injected:(Memory_venue.injected venue : Memory_venue.Injected.t)];
  [%expect
    {|
    ((plans 1) (placed 0) (strategy_sees 6)
     (injected
      ((rejects 0) (late_cancels 0) (split_fills 0) (stale_polls 0)
       (late_places 1))))
    |}];
  let%bind (_ : Live.Report.t) = Live_memory.stop live in
  return ()
;;

let%expect_test "a place that rests after the loop re-placed: the extra copies are \
                 cancelled"
  =
  let%bind venue, live =
    start
      { Memory_venue.Bugs.none with
        late_places = 1
      ; late_place_delay = Time_ns.Span.of_int_ms 250
      }
  in
  let%bind () =
    wait_until ~what:"duplicates cancelled" (fun () ->
      (Live_memory.report live).orders_cancelled >= 6)
  in
  print_orders (Memory_venue.orders venue);
  [%expect
    {|
    Sell 0.001@100070 gen 1
    Sell 0.001@100050 gen 1
    Sell 0.001@100030 gen 1
    Buy 0.001@99970 gen 1
    Buy 0.001@99950 gen 1
    Buy 0.001@99930 gen 1
    |}];
  let%bind (_ : Live.Report.t) = Live_memory.stop live in
  return ()
;;

let%expect_test "seeded bugs: rejects, late cancel acks, split fills and stale polls" =
  let bugs =
    { Memory_venue.Bugs.seed = 7
    ; reject_percent = 20
    ; cancel_ack_delay = Time_ns.Span.of_int_ms 20
    ; split_fills = 3
    ; stale_poll_percent = 20
    ; late_places = 0
    ; late_place_delay = Time_ns.Span.zero
    ; place_latency = Time_ns.Span.zero
    }
  in
  let%bind venue, live = start bugs in
  (* The mid walks up and down; every other step one of our quotes is taken. *)
  let mids = [ 0; 5; 12; 20; 12; 4; -6; -15; -25; -15; -5; 0; 8; 0; -8; 0 ] in
  let%bind () =
    Deferred.List.iteri ~how:`Sequential mids ~f:(fun i offset ->
      let mid = 100_000 + offset in
      Memory_venue.push_book
        venue
        (live_book ~bid:(Int.to_string (mid - 10)) ~ask:(Int.to_string (mid + 10)) ());
      if i % 2 = 1
      then (
        match Memory_venue.orders venue with
        | o :: _ -> Memory_venue.fill_resting venue o ~tid:(i + 1)
        | [] -> ());
      Clock_ns.after (Time_ns.Span.of_int_ms 60))
  in
  let%bind report = Live_memory.stop live in
  let%bind () = Clock_ns.after (Time_ns.Span.of_int_ms 50) in
  let injected = Memory_venue.injected venue in
  let position = report.position in
  let within_limit = Size.(abs position <= params.max_position) in
  print_s
    [%message
      ""
        ~left_resting:(List.length (Memory_venue.orders venue) : int)
        ~mode:(report.mode : Strategy.Mode.t)
        (within_limit : bool)
        ~some_fills:(report.fills > 0 : bool)
        ~some_rejected:(report.orders_rejected > 0 : bool)
        ~every_bug_seen:
          (injected.rejects > 0
           && injected.late_cancels > 0
           && injected.split_fills > 0
           && injected.stale_polls > 0
           : bool)];
  [%expect
    {|
    ((left_resting 0) (mode Running) (within_limit true) (some_fills true)
     (some_rejected true) (every_bug_seen true))
    |}];
  return ()
;;
