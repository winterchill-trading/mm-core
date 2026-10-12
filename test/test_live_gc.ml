open! Core
open! Async
open Mm_core
open Live_helpers
module Live_memory = Live.Make (Memory_venue)

let%expect_test "the report counts collections since the loop started" =
  Log.Global.set_output [];
  let%bind venue = Memory_venue.connect Memory_venue.Bugs.none >>| ok_exn in
  Memory_venue.push_book venue (live_book ());
  let%bind live =
    Live_memory.start
      venue
      { params = Params.btc; summary_interval = Time_ns.Span.of_int_sec 60 }
    >>| ok_exn
  in
  let before = (Live_memory.report live).gc in
  Gc.full_major ();
  Gc.full_major ();
  let after = (Live_memory.report live).gc in
  let%bind report = Live_memory.stop live in
  print_s
    [%message
      ""
        ~two_more_major:(after.major_collections - before.major_collections >= 2 : bool)
        ~minor_grew:(after.minor_collections > before.minor_collections : bool)
        ~heap_positive:(report.gc.heap_words > 0 : bool)];
  [%expect {| ((two_more_major true) (minor_grew true) (heap_positive true)) |}];
  let line =
    List.find_exn
      (String.split_lines (Live.Report.to_string_hum report))
      ~f:(fun l -> String.is_prefix l ~prefix:"gc ")
  in
  (* the numbers vary from run to run *)
  String.split line ~on:' '
  |> List.map ~f:(fun word ->
    if (not (String.is_empty word)) && String.for_all word ~f:Char.is_digit
    then "N"
    else word)
  |> String.concat ~sep:" "
  |> print_endline;
  [%expect {| gc               N minor, N major collections, heap N words |}];
  return ()
;;
