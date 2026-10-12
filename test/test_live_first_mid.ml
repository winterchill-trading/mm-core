open! Core
open! Async
open Exchange_core
open Mm_core
module Live_memory = Live.Make (Memory_venue)

(* [first_mid] races its read loop against a timeout; on timeout the loop must not be left
   reading the book feed. *)
let%expect_test "first_mid timeout closes the book feed and leaves no reader behind" =
  let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
  let time_source = Time_source.read_only ts in
  let venue = Memory_venue.create ~time_source Memory_venue.Bugs.none in
  let symbol = Symbol.of_string "BTC" in
  let book ~bids ~asks =
    let level px = { L2.Level.px = Price.of_int px; sz = Size.one; n = 1 } in
    { L2.symbol; time = 0; bids = List.map bids ~f:level; asks = List.map asks ~f:level }
  in
  let mid =
    Live_memory.first_mid ~time_source ~timeout:(Time_ns.Span.of_int_sec 1) venue symbol
  in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  (* one-sided books do not answer *)
  Memory_venue.push_book venue (book ~bids:[ 99 ] ~asks:[]);
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%message "" ~open_feeds:(Memory_venue.open_book_feeds venue : int)];
  [%expect {| (open_feeds 1) |}];
  let%bind () = Time_source.advance_by_alarms_by ts (Time_ns.Span.of_int_sec 1) in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%sexp (Deferred.peek mid : Price.t Or_error.t option)];
  print_s [%message "" ~open_feeds:(Memory_venue.open_book_feeds venue : int)];
  [%expect
    {|
    ((Error ("no two-sided book" (symbol BTC) (timeout 1s))))
    (open_feeds 0)
    |}];
  (* a book after the timeout goes nowhere *)
  Memory_venue.push_book venue (book ~bids:[ 99 ] ~asks:[ 101 ]);
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  print_s [%message "" ~open_feeds:(Memory_venue.open_book_feeds venue : int)];
  [%expect {| (open_feeds 0) |}];
  (* and a later call still answers *)
  let mid = Live_memory.first_mid ~time_source venue symbol in
  let%bind () = Scheduler.yield_until_no_jobs_remain () in
  Memory_venue.push_book venue (book ~bids:[ 99 ] ~asks:[ 101 ]);
  let%bind mid in
  print_s
    [%message
      "" (mid : Price.t Or_error.t) ~open_feeds:(Memory_venue.open_book_feeds venue : int)];
  [%expect {| ((mid (Ok 100)) (open_feeds 0)) |}];
  return ()
;;
