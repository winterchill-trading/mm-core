open! Core
open Exchange_core
open Mm_core
module Float_u = Stdlib_upstream_compatible.Float_u

let%test_unit "unboxed conversions match the boxed ones" =
  Quickcheck.test
    (Quickcheck.Generator.tuple3
       (Int.gen_incl (-1_000_000_000_000_000) 1_000_000_000_000_000)
       (Int.gen_incl (-1_000_000_000_000_000) 1_000_000_000_000_000)
       (Float.gen_incl (-500.) 500.))
    ~sexp_of:[%sexp_of: int * int * float]
    ~f:(fun (p, s, bps) ->
      let px = Price.of_units p
      and sz = Size.of_units s in
      [%test_eq: float] (Float_u.to_float (Fx.price_to_float_u px)) (Price.to_float px);
      [%test_eq: float] (Float_u.to_float (Fx.size_to_float_u sz)) (Size.to_float sz);
      [%test_eq: Price.t]
        (Fx.price_of_float_u (Fx.price_to_float_u px))
        ((Price.of_float [@alert "-lossy"]) (Price.to_float px));
      [%test_eq: Price.t] (Fx.shift_bps_u px (Float_u.of_float bps)) (Fx.shift_bps px bps))
;;

let%expect_test "price_of_float_u raises on NaN like Price.of_float" =
  let raises f = Result.is_error (Result.try_with f) in
  printf
    "%b %b\n"
    (raises (fun () -> Fx.price_of_float_u (Float_u.of_float Float.nan)))
    (raises (fun () -> (Price.of_float [@alert "-lossy"]) Float.nan));
  [%expect {| true true |}]
;;
