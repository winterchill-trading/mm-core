open! Core
open! Async
open Exchange_core
open Mm_core
module Live_memory = Live.Make (Memory_venue)

let%expect_test "tick-to-order latency on a synthetic clock" =
  Log.Global.set_output [];
  let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
  let time_source = Time_source.read_only ts in
  (* Every place takes 3ms to answer, so books arriving meanwhile wait for the sequencer. *)
  let venue =
    Memory_venue.create
      ~time_source
      { Memory_venue.Bugs.none with place_latency = Time_ns.Span.of_int_ms 3 }
  in
  let params = { Params.btc with max_actions_per_sec = 10_000 } in
  let%bind live =
    Live_memory.start
      ~time_source
      venue
      { params; summary_interval = Time_ns.Span.of_int_sec 3600 }
    >>| ok_exn
  in
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
  (* (arrival in ms, mid). Every tick sees the mid 5 bps away from the last quoted one, so
     each requotes:
     - 0: idle loop, sent at once (0ms); the place answers at 3
     - 1: waits for it, sent at 3 (2ms); answers at 6
     - 4 and 5: one tick at 6 for both, timed from the first (2ms); answers at 9
     - 50: sent at once (0ms); 52: waits until 53 (1ms)
     - 200: sent at once (0ms) *)
  let books =
    [ 0, 100_000
    ; 1, 100_050
    ; 4, 100_100
    ; 5, 100_000
    ; 50, 100_050
    ; 52, 100_000
    ; 200, 100_050
    ]
  in
  let%bind () =
    Deferred.List.iter ~how:`Sequential books ~f:(fun (at, mid) ->
      let%bind () = advance_to at in
      push_book mid;
      Scheduler.yield_until_no_jobs_remain ())
  in
  let%bind () = advance_to 300 in
  print_s [%sexp (Live_memory.tick_to_order live : Latency_histogram.Stats.t option)];
  [%expect {| (((count 6) (p50 0s) (p99 2ms) (max 2ms))) |}];
  let stopped = Live_memory.stop live in
  let%bind () = advance_to 2_000 in
  let%bind (_ : Live.Report.t) = stopped in
  return ()
;;
