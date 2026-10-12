open! Core
open Exchange_core
open Mm_core
open Helpers

let params = Params.btc

(* Wide book so the touch never clamps the ladder: mid 100000, spread 200. *)
let market ?(vol_bps = 0.) () =
  Market_state.on_book
    (Market_state.create ~vol_window:params.vol_window)
    (book ~bid:"99900" ~ask:"100100" ())
  |> fun m -> if Float.(vol_bps > 0.) then Market_state.with_vol_bps m vol_bps else m
;;

let inventory position =
  Inventory.on_fill
    Inventory.empty
    (fill
       ~tid:1
       ~side:(if String.is_prefix position ~prefix:"-" then Sell else Buy)
       ~px:"100000"
       ~sz:(String.chop_prefix_if_exists position ~prefix:"-")
       ())
;;

let show ?(params = params) ?(vol_bps = 0.) position =
  let market = market ~vol_bps () in
  let inv = inventory position in
  let m = Quoter.model params market inv |> Option.value_exn in
  printf
    "q=%.2f reservation=%s half_spread=%.2fbps\n"
    m.inventory_ratio
    (Price.to_string m.reservation)
    m.half_spread_bps;
  let quotes = Quoter.quote params market inv in
  let side s = List.filter quotes ~f:(fun (q : Quote.t) -> Side.equal q.side s) in
  let bids = side Buy
  and asks = side Sell in
  for level = 0 to params.levels - 1 do
    let cell l =
      match List.nth l level with
      | Some (q : Quote.t) ->
        sprintf "%7s @ %-7s" (Size.to_string q.sz) (Price.to_string q.px)
      | None -> sprintf "%17s" "-"
    in
    printf "L%d  bid %s | ask %s\n" level (cell bids) (cell asks)
  done
;;

let%expect_test "symmetric at zero inventory" =
  show "0";
  [%expect
    {|
    q=0.00 reservation=100000 half_spread=3.00bps
    L0  bid   0.001 @ 99970   | ask   0.001 @ 100030
    L1  bid   0.001 @ 99950   | ask   0.001 @ 100050
    L2  bid   0.001 @ 99930   | ask   0.001 @ 100070
    |}]
;;

let%expect_test "long skews quotes down, short skews them up" =
  show "0.005";
  [%expect
    {|
    q=0.50 reservation=99980 half_spread=3.00bps
    L0  bid   0.001 @ 99950   | ask   0.001 @ 100010
    L1  bid   0.001 @ 99930   | ask   0.001 @ 100030
    L2  bid   0.001 @ 99910   | ask   0.001 @ 100050
    |}];
  show "-0.005";
  [%expect
    {|
    q=-0.50 reservation=100020 half_spread=3.00bps
    L0  bid   0.001 @ 99989   | ask   0.001 @ 100051
    L1  bid   0.001 @ 99969   | ask   0.001 @ 100071
    L2  bid   0.001 @ 99949   | ask   0.001 @ 100091
    |}]
;;

let%expect_test "spread widens with volatility" =
  show "0" ~vol_bps:2.;
  [%expect
    {|
    q=0.00 reservation=100000 half_spread=4.00bps
    L0  bid   0.001 @ 99960   | ask   0.001 @ 100040
    L1  bid   0.001 @ 99940   | ask   0.001 @ 100060
    L2  bid   0.001 @ 99920   | ask   0.001 @ 100080
    |}];
  show "0" ~vol_bps:5.;
  [%expect
    {|
    q=0.00 reservation=100000 half_spread=9.25bps
    L0  bid   0.001 @ 99907   | ask   0.001 @ 100093
    L1  bid   0.001 @ 99887   | ask   0.001 @ 100113
    L2  bid   0.001 @ 99867   | ask   0.001 @ 100133
    |}]
;;

let%expect_test "sizes shrink near the limit, side suppressed at max position" =
  show "0.0085";
  [%expect
    {|
    q=0.85 reservation=99966 half_spread=3.00bps
    L0  bid   0.001 @ 99936   | ask   0.001 @ 99996
    L1  bid  0.0005 @ 99916   | ask   0.001 @ 100016
    L2  bid                 - | ask   0.001 @ 100036
    |}];
  show "0.01";
  [%expect
    {|
    q=1.00 reservation=99960 half_spread=3.00bps
    L0  bid                 - | ask   0.001 @ 99990
    L1  bid                 - | ask   0.001 @ 100010
    L2  bid                 - | ask   0.001 @ 100030
    |}];
  show "-0.01";
  [%expect
    {|
    q=-1.00 reservation=100040 half_spread=3.00bps
    L0  bid   0.001 @ 100009  | ask                 -
    L1  bid   0.001 @ 99989   | ask                 -
    L2  bid   0.001 @ 99969   | ask                 -
    |}]
;;

let%expect_test "notional limit tightens capacity" =
  show "0" ~params:{ params with max_notional = usd "250" };
  [%expect
    {|
    q=0.00 reservation=100000 half_spread=3.00bps
    L0  bid   0.001 @ 99970   | ask   0.001 @ 100030
    L1  bid   0.001 @ 99950   | ask   0.001 @ 100050
    L2  bid  0.0005 @ 99930   | ask  0.0005 @ 100070
    |}]
;;

let%expect_test "quotes stay behind the touch" =
  let market =
    Market_state.on_book
      (Market_state.create ~vol_window:10)
      (book ~bid:"100000" ~ask:"100001" ~bid_sz:"5" ())
  in
  print_s [%sexp (Quoter.quote params market (inventory "0.009") : Quote.t list)];
  [%expect
    {|
    (((side Buy) (level 0) (px 99934) (sz 0.001))
     ((side Sell) (level 0) (px 100001) (sz 0.001))
     ((side Sell) (level 1) (px 100015) (sz 0.001))
     ((side Sell) (level 2) (px 100035) (sz 0.001)))
    |}]
;;

let%expect_test "no quotes without a two-sided book" =
  print_s
    [%sexp
      (Quoter.quote params (Market_state.create ~vol_window:10) Inventory.empty
       : Quote.t list)];
  [%expect {| () |}]
;;
