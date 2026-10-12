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

let instrument =
  Instrument.create
    (Symbol.of_string "BTCUSDT")
    ~size_decimals:3
    ~price_tick:(Price.of_string "0.1")
    ~max_leverage:20
;;

let show_size ?min_notional mid =
  let mid = Price.of_string mid in
  let params = Params.for_instrument ?min_notional instrument ~mid in
  print_s
    [%message
      ""
        ~order_size:(params.order_size : Size.t)
        ~notional:(Fx.notional mid params.order_size : Usd.t)]
;;

let%expect_test "order size without a venue minimum is about 100 USD" =
  show_size "100000";
  [%expect {| ((order_size 0.001) (notional 100)) |}];
  show_size "60000";
  [%expect {| ((order_size 0.002) (notional 120)) |}]
;;

let%expect_test "order size clears the venue minimum with a margin, rounded up" =
  (* 1.2 x 100 USD = 120 USD: 0.0012 BTC, rounded up to the 0.001 lot *)
  show_size ~min_notional:(Usd.of_int 100) "100000";
  [%expect {| ((order_size 0.002) (notional 200)) |}];
  show_size ~min_notional:(Usd.of_int 100) "120000";
  [%expect {| ((order_size 0.001) (notional 120)) |}];
  show_size ~min_notional:(Usd.of_int 100) "119999";
  [%expect {| ((order_size 0.002) (notional 239.998)) |}];
  (* a small minimum leaves the ~100 USD default alone *)
  show_size ~min_notional:(Usd.of_int 5) "100000";
  [%expect {| ((order_size 0.001) (notional 100)) |}]
;;
