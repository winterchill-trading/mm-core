open! Core
open Mm_core

(* A scripted morning: a quiet market, a rally, a sell-off through the bids, then calm. *)
let script =
  [ 0, "99990", "100010"
  ; 500, "99995", "100015"
  ; 1_000, "99990", "100010"
  ; 1_500, "100010", "100030"
  ; 2_000, "100040", "100060"
  ; 2_500, "100080", "100100"
  ; 3_000, "100070", "100090"
  ; 3_500, "100030", "100050"
  ; 4_000, "99960", "99980"
  ; 4_500, "99900", "99920"
  ; 5_000, "99920", "99940"
  ; 5_500, "99950", "99970"
  ; 6_000, "99960", "99980"
  ; 6_500, "99955", "99975"
  ; 7_000, "99960", "99980"
  ; 7_500, "99970", "99990"
  ; 8_000, "99980", "100000"
  ; 8_500, "99990", "100010"
  ; 9_000, "99985", "100005"
  ; 9_500, "99990", "100010"
  ]
;;

let params = { Params.btc with requote_interval_ms = Time_ns.Span.of_int_ms 500 }

let%expect_test "stepped run, seed 1" =
  Stepped.print (Stepped.run ~seed:1 params script);
  [%expect
    {|
       0  99990/100010 | fills -                                 | c0 p6 | pos 0       pnl 0       Running
     500  99995/100015 | fills -                                 | c0 p0 | pos 0       pnl 0       Running
    1000  99990/100010 | fills -                                 | c0 p0 | pos 0       pnl 0       Running
    1500 100010/100030 | fills -                                 | c2 p2 | pos 0       pnl 0       Running
    2000 100040/100060 | fills -                                 | c3 p3 | pos 0       pnl 0       Running
    2500 100080/100100 | fills S 0.001@100102                    | c3 p4 | pos -0.001  pnl 0.012   Running
    3000 100070/100090 | fills -                                 | c0 p0 | pos -0.001  pnl 0.022   Running
    3500 100030/100050 | fills B 0.001@100062 B 0.001@100042     | c4 p6 | pos 0.001   pnl 0.038   Running
    4000   99960/99980 | fills B 0.001@100002 B 0.001@99982      | c4 p6 | pos 0.003   pnl -0.076  Running
    4500   99900/99920 | fills -                                 | c6 p6 | pos 0.003   pnl -0.256  Running
    5000   99920/99940 | fills -                                 | c2 p2 | pos 0.003   pnl -0.196  Running
    5500   99950/99970 | fills S 0.001@99951                     | c3 p4 | pos 0.002   pnl -0.115  Running
    6000   99960/99980 | fills -                                 | c1 p1 | pos 0.002   pnl -0.095  Running
    6500   99955/99975 | fills S 0.001@99991                     | c0 p1 | pos 0.001   pnl -0.079  Running
    7000   99960/99980 | fills -                                 | c2 p2 | pos 0.001   pnl -0.074  Running
    7500   99970/99990 | fills -                                 | c2 p2 | pos 0.001   pnl -0.064  Running
    8000  99980/100000 | fills S 0.001@100007                    | c1 p2 | pos 0       pnl -0.037  Running
    8500  99990/100010 | fills -                                 | c2 p2 | pos 0       pnl -0.037  Running
    9000  99985/100005 | fills -                                 | c1 p1 | pos 0       pnl -0.037  Running
    9500  99990/100010 | fills -                                 | c0 p0 | pos 0       pnl -0.037  Running
    |}]
;;

let%expect_test "the same seed reproduces the run; another seed diverges" =
  let fills seed =
    Stepped.run ~seed params script
    |> List.concat_map ~f:(fun (s : Stepped.Step.t) -> s.fills)
  in
  print_s
    [%message
      ""
        ~same:([%equal: Fill.t list] (fills 1) (fills 1) : bool)
        ~different:(not ([%equal: Fill.t list] (fills 1) (fills 2)) : bool)];
  [%expect {| ((same true) (different true)) |}]
;;
