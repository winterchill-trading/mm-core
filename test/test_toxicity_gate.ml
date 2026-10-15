open! Core
open Mm_core

let config = Toxicity_gate.Config.default
let sec s = s * 1_000_000_000

let decide ?(now = sec 10) ?(at = sec 10) ?(imbalance = 0.6) value =
  print_s
    [%sexp
      (Toxicity_gate.decide config ~now (Some { value; imbalance; time = at })
       : Toxicity_gate.Decision.t)]
;;

let%expect_test "normal, widening with skew towards the flow, pulled, stale" =
  print_s [%sexp (Toxicity_gate.decide config ~now:0 None : Toxicity_gate.Decision.t)];
  decide 0.5;
  decide 0.8;
  decide 0.875;
  decide 0.875 ~imbalance:(-1.);
  decide 0.95;
  decide 0.99 ~at:(sec 10) ~now:(sec 71);
  decide Float.nan;
  [%expect
    {|
    Normal
    Normal
    (Widen (intensity 0) (extra_half_spread_bps 0) (reservation_shift_bps 0)
     (size_factor 1))
    (Widen (intensity 0.5) (extra_half_spread_bps 2.5)
     (reservation_shift_bps 0.6) (size_factor 0.75))
    (Widen (intensity 0.5) (extra_half_spread_bps 2.5) (reservation_shift_bps -1)
     (size_factor 0.75))
    (Pull (signal 0.95))
    Normal
    Normal
    |}]
;;

let%expect_test "events on a change of kind only" =
  let d v : Toxicity_gate.Decision.t =
    Toxicity_gate.decide config ~now:0 (Some { value = v; imbalance = 0.; time = 0 })
  in
  List.iter
    [ 0.1, 0.85; 0.85, 0.9; 0.9, 0.97; 0.97, 0.85; 0.85, 0.2; 0.2, 0.3 ]
    ~f:(fun (a, b) ->
      print_s
        [%sexp
          (a : float)
          , (b : float)
          , (Toxicity_gate.Event.of_change ~before:(d a) ~after:(d b) ~signal:b
             : Toxicity_gate.Event.t option)]);
  [%expect
    {|
    (0.1 0.85 ((Widening (signal 0.85))))
    (0.85 0.9 ())
    (0.9 0.97 ((Pulled (signal 0.97))))
    (0.97 0.85 ((Widening (signal 0.85))))
    (0.85 0.2 (Cleared))
    (0.2 0.3 ())
    |}]
;;

let%expect_test "validation" =
  List.iter
    [ config
    ; { config with widen_from = 0.95 }
    ; { config with size_cut = 1. }
    ; { config with skew_bps = Float.nan }
    ]
    ~f:(fun c ->
      print_s
        [%sexp
          (Toxicity_gate.Config.validate c |> Result.map ~f:ignore : unit Or_error.t)]);
  [%expect
    {|
    (Ok ())
    (Error
     ("invalid toxicity gate" ("widen_from must be below pull_at")
      (config
       ((widen_from 0.95) (pull_at 0.95) (max_extra_half_spread_bps 5)
        (skew_bps 2) (size_cut 0.5) (max_signal_age_ms 1m)))))
    (Error
     ("invalid toxicity gate" ("size_cut outside [0, 1)")
      (config
       ((widen_from 0.8) (pull_at 0.95) (max_extra_half_spread_bps 5)
        (skew_bps 2) (size_cut 1) (max_signal_age_ms 1m)))))
    (Error
     ("invalid toxicity gate" ("every float must be finite")
      (config
       ((widen_from 0.8) (pull_at 0.95) (max_extra_half_spread_bps 5)
        (skew_bps NAN) (size_cut 0.5) (max_signal_age_ms 1m)))))
    |}]
;;
