open! Core
open Mm_core

let show h =
  print_s [%sexp (Latency_histogram.stats h : Latency_histogram.Stats.t option)]
;;

let%expect_test "percentiles are bucket bounds, the maximum is exact" =
  let h = Latency_histogram.create () in
  show h;
  [%expect {| () |}];
  (* 1..100 ms: p50 is 50ms, p99 is 99ms. *)
  for ms = 1 to 100 do
    Latency_histogram.record h (Time_ns.Span.of_int_ms ms)
  done;
  show h;
  [%expect {| (((count 100) (p50 50ms) (p99 99ms) (max 100ms))) |}];
  (* Three significant digits round up to two; the max keeps them. *)
  Latency_histogram.record h (Time_ns.Span.of_int_us 123_456);
  print_s [%sexp (Latency_histogram.percentile h 1. : Time_ns.Span.t option)];
  show h;
  [%expect
    {|
    (123.456ms)
    (((count 101) (p50 51ms) (p99 100ms) (max 123.456ms)))
    |}];
  Latency_histogram.record h (Time_ns.Span.of_int_ns (-5));
  Latency_histogram.record h (Time_ns.Span.of_int_ns 42);
  print_s [%sexp (Latency_histogram.percentile h 0. : Time_ns.Span.t option)];
  [%expect {| (0s) |}];
  print_endline
    (Latency_histogram.Stats.to_string_hum (Option.value_exn (Latency_histogram.stats h)));
  [%expect {| n=103 p50=50ms p99=100ms max=123ms |}]
;;

let%test_unit "a percentile is never below the exact one nor 10% above it" =
  Quickcheck.test
    (List.gen_non_empty (Int.gen_incl 0 10_000_000_000))
    ~sexp_of:[%sexp_of: int list]
    ~f:(fun samples ->
      let h = Latency_histogram.create () in
      List.iter samples ~f:(fun ns ->
        Latency_histogram.record h (Time_ns.Span.of_int_ns ns));
      let sorted = List.sort samples ~compare:Int.compare |> Array.of_list in
      List.iter [ 0.5; 0.9; 0.99; 1. ] ~f:(fun q ->
        let rank =
          Int.max 1 (Float.iround_up_exn (q *. Float.of_int (Array.length sorted)))
        in
        let exact = sorted.(rank - 1) in
        let got =
          Time_ns.Span.to_int_ns (Option.value_exn (Latency_histogram.percentile h q))
        in
        if got < exact || Float.(of_int got > (1.1 *. of_int exact) +. 1.)
        then raise_s [%message "percentile off" (q : float) (exact : int) (got : int)]))
;;
