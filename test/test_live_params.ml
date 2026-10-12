open! Core
open! Async
open Exchange_core
open Mm_core
module Live_memory = Live.Make (Memory_venue)

let print_orders venue =
  Live_helpers.print_orders (Memory_venue.orders venue);
  printf "cancel_all calls: %d\n" (Memory_venue.cancel_all_calls venue)
;;

let%expect_test "update_params moves quotes on the next requote without a cancel-all" =
  Log.Global.set_output [];
  let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
  let time_source = Time_source.read_only ts in
  let venue = Memory_venue.create ~time_source Memory_venue.Bugs.none in
  let params = { Params.btc with requote_interval_ms = 100 } in
  let%bind live =
    Live_memory.start
      ~time_source
      venue
      { params; summary_interval = Time_ns.Span.of_int_sec 3600 }
    >>| ok_exn
  in
  let push_book () =
    Memory_venue.push_book
      venue
      { symbol = params.symbol
      ; time = Time_ns.to_int_ns_since_epoch (Time_source.now ts)
      ; bids = [ { px = Price.of_int 99_990; sz = Size.one; n = 1 } ]
      ; asks = [ { px = Price.of_int 100_010; sz = Size.one; n = 1 } ]
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
  push_book ();
  let%bind () = advance_to 50 in
  print_orders venue;
  [%expect
    {|
    Sell 0.001@100070 gen 0
    Sell 0.001@100050 gen 0
    Sell 0.001@100030 gen 0
    Buy 0.001@99970 gen 0
    Buy 0.001@99950 gen 0
    Buy 0.001@99930 gen 0
    cancel_all calls: 0
    |}];
  let plans = (Live_memory.report live).plans in
  Live_memory.update_params
    live
    { params with base_half_spread_bps = 5.; requote_interval_ms = 200 }
  |> ok_exn;
  let%bind () = advance_to 60 in
  print_orders venue;
  print_s
    [%message
      ""
        ~new_plans:((Live_memory.report live).plans - plans : int)
        ~half_spread:
          ((Strategy.params (Live_memory.strategy live)).base_half_spread_bps : float)
        ~mode:(Strategy.mode (Live_memory.strategy live) : Strategy.Mode.t)];
  [%expect
    {|
    Sell 0.001@100090 gen 1
    Sell 0.001@100070 gen 0
    Sell 0.001@100050 gen 0
    Buy 0.001@99950 gen 0
    Buy 0.001@99930 gen 0
    Buy 0.001@99910 gen 1
    cancel_all calls: 0
    ((new_plans 1) (half_spread 5) (mode Running))
    |}];
  print_s
    [%sexp
      (Live_memory.update_params live { params with symbol = Symbol.of_string "ETH" }
       : unit Or_error.t)];
  [%expect {| (Error ("update_params cannot change the symbol" (from BTC) (to_ ETH))) |}];
  let stopped = Live_memory.stop live in
  let%bind () = advance_to 2_000 in
  let%bind (_ : Live.Report.t) = stopped in
  printf "cancel_all calls after stop: %d\n" (Memory_venue.cancel_all_calls venue);
  [%expect {| cancel_all calls after stop: 1 |}];
  return ()
;;

let%expect_test "connect_and_start returns the first error and no handle" =
  Log.Global.set_output [];
  let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
  let started =
    Live_memory.connect_and_start
      ~time_source:(Time_source.read_only ts)
      Memory_venue.Bugs.none
      ~symbol:Params.btc.symbol
      ~params:(fun _ ~mid:_ -> Ok Params.btc)
      ~summary_interval:(Time_ns.Span.of_int_sec 60)
  in
  let%bind () =
    Time_source.advance_by_alarms
      ts
      ~to_:(Time_ns.of_int_ns_since_epoch (11 * 1_000_000_000))
  in
  let%bind started in
  print_s [%sexp (Result.map started ~f:(fun _ -> ()) : unit Or_error.t)];
  [%expect {| (Error ("no two-sided book" (symbol BTC) (timeout 10s))) |}];
  return ()
;;

let%expect_test "a reconnect burst stays inside the grace period; a later one trips" =
  Log.Global.set_output [];
  let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
  let time_source = Time_source.read_only ts in
  let venue = Memory_venue.create ~time_source Memory_venue.Bugs.none in
  let params =
    { Params.btc with max_actions_per_sec = 40; action_rate_grace_ms = 1_000 }
  in
  let%bind live =
    Live_memory.start
      ~time_source
      venue
      { params; summary_interval = Time_ns.Span.of_int_sec 3600 }
    >>| ok_exn
  in
  let advance_to ms =
    let%bind () =
      Time_source.advance_by_alarms
        ts
        ~to_:(Time_ns.of_int_ns_since_epoch (ms * 1_000_000))
    in
    Scheduler.yield_until_no_jobs_remain ()
  in
  (* A book whose mid flips by 10 bps every 2 ms, so each tick replaces the ladder. *)
  let burst ~from ~until =
    Deferred.List.iter ~how:`Sequential (List.range from until ~stride:2) ~f:(fun at ->
      let%bind () = advance_to at in
      let mid = if at / 2 % 2 = 0 then 100_000 else 100_100 in
      Memory_venue.push_book
        venue
        { symbol = params.symbol
        ; time = Time_ns.to_int_ns_since_epoch (Time_source.now ts)
        ; bids = [ { px = Price.of_int (mid - 10); sz = Size.one; n = 1 } ]
        ; asks = [ { px = Price.of_int (mid + 10); sz = Size.one; n = 1 } ]
        };
      Scheduler.yield_until_no_jobs_remain ())
  in
  let show label =
    let r = Live_memory.report live in
    print_s
      [%message
        label
          ~plans:(r.plans : int)
          ~reconnects:(r.reconnects : int)
          ~mode:(r.mode : Strategy.Mode.t)]
  in
  let%bind () = advance_to 10_000 in
  Memory_venue.simulate_reconnect venue;
  let%bind () = burst ~from:10_000 ~until:10_200 in
  show "burst after the reconnect";
  [%expect {| ("burst after the reconnect" (plans 100) (reconnects 1) (mode Running)) |}];
  let%bind () = burst ~from:12_000 ~until:12_200 in
  show "burst outside the grace period";
  [%expect
    {|
    ("burst outside the grace period" (plans 104) (reconnects 1)
     (mode (Halted (Max_action_rate (actions 48)))))
    |}];
  let stopped = Live_memory.stop live in
  let%bind () = advance_to 20_000 in
  let%bind (_ : Live.Report.t) = stopped in
  return ()
;;
