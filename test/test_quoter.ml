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

let%expect_test "reasons for sides without quotes" =
  let reasons ?(params = params) market inv =
    print_s
      [%sexp (Quoter.unquoted params market inv : (Side.t * Quoter.Unquoted.t) list)]
  in
  reasons (Market_state.create ~vol_window:10) Inventory.empty;
  [%expect {| ((Buy No_book) (Sell No_book)) |}];
  reasons (market ()) (inventory "0");
  [%expect {| () |}];
  reasons (market ()) (inventory "0.01");
  [%expect {| ((Buy (At_limit (position 0.01) (capacity 0.01)))) |}];
  reasons (market ()) (inventory "-0.009999");
  [%expect {| ((Sell (Room_below_lot (room 0.000001)))) |}];
  (* A half-spread of 100% puts every bid at zero. *)
  reasons
    ~params:{ params with base_half_spread_bps = 10_000. }
    (market ())
    (inventory "0");
  [%expect {| ((Buy Non_positive_price)) |}]
;;

let%expect_test "explain a long position" =
  print_string
    (Quoter.For_testing.explain params (market ~vol_bps:4. ()) (inventory "0.008"));
  [%expect
    {|
    quote
      position 0.008 (target 0)
      book bid 99900 ask 100100
      mid 100000, microprice 100000
      vol 4.0000 bps (estimate 4.0000, cap 10.0000)
      capacity 0.01 = min(max_position 0.01, max_notional 1500 / mid), floored to lot 0.00001
      inventory ratio q 0.8000
      risk term v = gamma 0.5000 * vol^2 = 8.0000 bps
      reservation 99904 = microprice shifted by -q * (skew 4.0000 + v) = -9.6000 bps
      half-spread 7.0000 bps = base 3.0000 + v / 2
      bids: room 0.002
        L0 0.001 @ 99834: distance 7.0000 bps, target 99834.0672, on tick 99834
        L1 0.001 @ 99814: distance 9.0000 bps, target 99814.0864, on tick 99814
      asks: room 0.018
        L0 0.001 @ 99974: distance 7.0000 bps, target 99973.9328, on tick 99974
        L1 0.001 @ 99994: distance 9.0000 bps, target 99993.9136, on tick 99994
        L2 0.001 @ 100014: distance 11.0000 bps, target 100013.8944, on tick 100014
    |}]
;;

(* One fixed situation (mid at the instrument's price, a spread of 4 ticks, long 40% of
   the position limit, volatility 2 bps) under the parameters each preset builds. *)
let%expect_test "quotes under each parameter preset" =
  let presets =
    [ "btc", Params.btc, "100000"
    ; ( "BTCUSDT tick 0.1, min notional 100"
      , Params.for_instrument
          ~min_notional:(Usd.of_int 100)
          (Instrument.create
             (Symbol.of_string "BTCUSDT")
             ~size_decimals:3
             ~price_tick:(Price.of_string "0.1")
             ~max_leverage:20)
          ~mid:(px "100000")
      , "100000" )
    ; ( "ETH tick 0.01"
      , Params.for_instrument
          (Instrument.create
             (Symbol.of_string "ETH")
             ~size_decimals:3
             ~price_tick:(Price.of_string "0.01")
             ~max_leverage:20)
          ~mid:(px "3000")
      , "3000" )
    ; ( "1000PEPE tick 0.0000001"
      , Params.for_instrument
          (Instrument.create
             (Symbol.of_string "1000PEPE")
             ~size_decimals:0
             ~price_tick:(Price.of_string "0.0000001")
             ~max_leverage:10)
          ~mid:(px "0.0123456")
      , "0.0123456" )
    ]
  in
  List.iter presets ~f:(fun (name, (p : Params.t), mid) ->
    let mid = px mid in
    let two_ticks = Price.scale p.tick ~by:2 in
    let book : L2.t =
      { symbol = p.symbol
      ; time = 0
      ; bids = [ { px = Price.(mid - two_ticks); sz = Size.of_int 10; n = 1 } ]
      ; asks = [ { px = Price.(mid + two_ticks); sz = Size.of_int 10; n = 1 } ]
      }
    in
    let market =
      Market_state.with_vol_bps
        (Market_state.on_book (Market_state.create ~vol_window:p.vol_window) book)
        2.
    in
    let long =
      Fx.size_floor (Size.of_float (0.4 *. Size.to_float p.max_position)) ~lot:p.lot
    in
    let inv =
      Inventory.on_fill
        Inventory.empty
        { (fill ~tid:1 ~side:Buy ~px:"1" ~sz:"1" ()) with
          symbol = p.symbol
        ; px = mid
        ; size = long
        }
    in
    let quotes, reasons = Quoter.quote_with_reasons p market inv in
    printf
      "%s: order %s, max position %s, long %s\n"
      name
      (Size.to_string p.order_size)
      (Size.to_string p.max_position)
      (Size.to_string long);
    List.iter quotes ~f:(fun (q : Quote.t) ->
      printf
        "  %s L%d %s @ %s\n"
        (if Side.is_buy q.side then "bid" else "ask")
        q.level
        (Size.to_string q.sz)
        (Price.to_string q.px));
    List.iter reasons ~f:(fun r -> print_s [%sexp (r : Side.t * Quoter.Unquoted.t)]));
  [%expect
    {|
    btc: order 0.001, max position 0.01, long 0.004
      bid L0 0.001 @ 99936
      bid L1 0.001 @ 99916
      bid L2 0.001 @ 99896
      ask L0 0.001 @ 100016
      ask L1 0.001 @ 100036
      ask L2 0.001 @ 100056
    BTCUSDT tick 0.1, min notional 100: order 0.002, max position 0.02, long 0.008
      bid L0 0.002 @ 99936
      bid L1 0.002 @ 99916
      bid L2 0.002 @ 99896
      ask L0 0.002 @ 100016
      ask L1 0.002 @ 100036
      ask L2 0.002 @ 100056
    ETH tick 0.01: order 0.033, max position 0.33, long 0.132
      bid L0 0.033 @ 2998.08
      bid L1 0.033 @ 2997.48
      bid L2 0.033 @ 2996.88
      ask L0 0.033 @ 3000.48
      ask L1 0.033 @ 3001.08
      ask L2 0.033 @ 3001.68
    1000PEPE tick 0.0000001: order 8100, max position 81000, long 32400
      bid L0 8100 @ 0.0123377
      bid L1 8100 @ 0.0123352
      bid L2 8100 @ 0.0123327
      ask L0 8100 @ 0.0123476
      ask L1 8100 @ 0.0123501
      ask L2 8100 @ 0.0123526
    |}]
;;
