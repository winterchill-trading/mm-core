open! Core
open! Async
open Mm_core_replay

(* [fixtures/] is a copy of market-data's event-log-v1 Binance BTCUSDT L2 log: a snapshot
   (99.9 / 100.1) and, 21 ms later, a delta removing the best bid. *)
let dir = "fixtures"

let%expect_test "the strategy quotes the snapshot and requotes after the bid moves" =
  let%bind instrument, files = Replay.l2_files ~dir ~symbol:"BTCUSDT" >>| ok_exn in
  print_s
    [%message
      ""
        ~instrument:(Market_data.Instrument.to_string instrument : string)
        ~files:
          (List.map files ~f:(fun f -> String.chop_prefix_exn f ~prefix:"fixtures/")
           : string list)];
  [%expect
    {|
    ((instrument binance:BTCUSDT)
     (files (binance/BTCUSDT/norm/l2/2026-01-01.bin)))
    |}];
  let feed = Market_data.Replay_feed.of_files ~speed:0. files in
  let%bind strategy =
    Replay.run feed ~params:(Replay.catalogue_params instrument) ~f:(fun tick ->
      print_endline (Replay.Tick.to_string_hum tick))
    >>| ok_exn
  in
  [%expect
    {|
    2026-01-01T00:00:00.001000000Z bid 99.9 ask 100.1 Running
      place buy 1 @ 99.9
      place buy 1 @ 99.8
      place buy 1 @ 99.7
      place sell 1 @ 100
      place sell 1 @ 100.1
      place sell 1 @ 100.2
    2026-01-01T00:00:00.022000000Z bid 99.8 ask 100.1 Running
      cancel 4
      place sell 1 @ 100.3
      keep 5
    |}];
  print_s
    [%sexp
      (Option.map strategy ~f:(fun s -> List.length (Mm_core.Strategy.open_orders s))
       : int option)];
  [%expect {| (6) |}];
  return ()
;;

let%expect_test "unknown symbols and venue ambiguity are errors" =
  let show symbol =
    let%map result = Replay.l2_files ~dir ~symbol in
    print_s
      [%sexp
        (Result.map result ~f:(fun (_, files) -> List.length files) : int Or_error.t)]
  in
  let%bind () = show "ETHUSDT" in
  let%bind () = show "binance:BTCUSDT" in
  [%expect
    {|
    (Error ("no recorded instrument" (symbol ETHUSDT) (dir fixtures)))
    (Ok 1)
    |}];
  return ()
;;
