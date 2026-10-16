open! Core
open! Async
open Exchange_core
open Mm_core
module Live_memory = Live.Make (Memory_venue)

let%expect_test "the live loop pulls on a stale reference and reports the events" =
  Log.Global.set_output [];
  let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
  let time_source = Time_source.read_only ts in
  let venue = Memory_venue.create ~time_source Memory_venue.Bugs.none in
  let params =
    { Params.btc with
      levels = 1
    ; requote_interval_ms = Time_ns.Span.of_int_ms 100
    ; reference_guard =
        Some
          { Reference_guard.Config.default with
            grace_ms = Time_ns.Span.of_int_ms 500
          ; max_jump_bps = Some 200.
          }
    }
  in
  let%bind live =
    Live_memory.start
      ~time_source
      venue
      { params; summary_interval = Time_ns.Span.of_int_sec 3600 }
    >>| ok_exn
  in
  Live_memory.on_strategy_event live ~f:(fun event ->
    print_s
      [%message
        "event"
          ~at_ms:(Time_ns.to_int_ns_since_epoch (Time_source.now ts) / 1_000_000 : int)
          (event : Strategy.Event.t)]);
  let push_book mid =
    Memory_venue.push_book
      venue
      { symbol = params.symbol
      ; time = Time_ns.to_int_ns_since_epoch (Time_source.now ts)
      ; bids = [ { px = Price.of_int (mid - 10); sz = Size.one; n = 1 } ]
      ; asks = [ { px = Price.of_int (mid + 10); sz = Size.one; n = 1 } ]
      }
  in
  let advance_to ms =
    let%bind () =
      Time_source.advance_by_alarms
        ts
        ~to_:(Time_ns.of_int_ns_since_epoch (ms * 1_000_000))
    in
    Scheduler.yield_until_no_jobs_remain ()
  in
  let show () =
    print_s
      [%message
        ""
          ~mode:(Strategy.mode (Live_memory.strategy live) : Strategy.Mode.t)
          ~resting:(List.length (Memory_venue.orders venue) : int)]
  in
  (* No reference yet: the default guard pulls. *)
  push_book 100_000;
  let%bind () = advance_to 50 in
  show ();
  Live_memory.on_reference live (Price.of_int 100_000);
  let%bind () = advance_to 100 in
  show ();
  (* The book moves 1% while the reference stays: pulled after the 500 ms grace. *)
  let%bind () = advance_to 200 in
  push_book 101_000;
  let%bind () = advance_to 300 in
  show ();
  let%bind () = advance_to 800 in
  push_book 101_000;
  let%bind () = advance_to 850 in
  show ();
  (* A 3% reference jump pulls at once, without a grace period. *)
  Live_memory.on_reference live (Price.of_int 101_000);
  let%bind () = advance_to 900 in
  show ();
  Live_memory.on_reference live (Price.of_int 104_100);
  let%bind () = advance_to 950 in
  show ();
  (* [stop] waits on timers of the synthetic clock: advance it. *)
  let stopped = Live_memory.stop live in
  let%bind () = advance_to 5_000 in
  let%bind report = stopped in
  print_s [%message (report.mode : Strategy.Mode.t)];
  [%expect
    {|
    (event (at_ms 0) (event (Reference (Reference_missing (age_ms ())))))
    ((mode (Pulled (Reference_missing (age_ms ())))) (resting 0))
    (event (at_ms 50) (event (Reference Resumed)))
    ((mode Running) (resting 2))
    ((mode Running) (resting 2))
    (event (at_ms 700)
     (event
      (Reference
       (Diverged (divergence_bps 100) (held_ms 500) (venue_mid 101000)
        (reference_mid 100000)))))
    ((mode (Pulled (Reference_divergence (divergence_bps 100) (held_ms 500))))
     (resting 0))
    (event (at_ms 850) (event (Reference Resumed)))
    ((mode Running) (resting 2))
    (event (at_ms 900)
     (event
      (Reference
       (Reference_jumped (from_ 101000) (to_ 104100)
        (jump_bps 306.93069306930693)))))
    (event (at_ms 900)
     (event
      (Reference
       (Diverged (divergence_bps -297.79058597502404) (held_ms 0)
        (venue_mid 101000) (reference_mid 104100)))))
    ((mode
      (Pulled
       (Reference_divergence (divergence_bps -297.79058597502404) (held_ms 0))))
     (resting 0))
    (report.mode
     (Pulled
      (Reference_divergence (divergence_bps -297.79058597502404) (held_ms 0))))
    |}];
  return ()
;;
