open! Core
open Exchange_core
open Mm_core

let%expect_test "btc defaults validate" =
  print_s [%sexp (Params.validate Params.btc |> Or_error.is_ok : bool)];
  [%expect {| true |}]
;;

let%expect_test "invalid params list every problem" =
  let t = { Params.btc with tick = Price.zero; levels = 0; base_half_spread_bps = 0. } in
  print_s [%sexp (Params.validate t : Params.t Or_error.t)];
  [%expect
    {|
    (Error
     ("invalid params"
      (errors
       ("tick must be positive" "levels must be in [1, 255]"
        "base_half_spread_bps must be positive"))))
    |}]
;;
