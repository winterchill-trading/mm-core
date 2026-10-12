open! Core
open Exchange_core
open Mm_core
open Helpers

let params = Params.btc

let market ?(time = 0) mid =
  let m = Price.of_string mid in
  Market_state.on_book
    (Market_state.create ~vol_window:10)
    (book
       ~time
       ~bid:Price.(to_string (m - of_int 5))
       ~ask:Price.(to_string (m + of_int 5))
       ())
;;

let long ?(px = "100000") s =
  Inventory.on_fill Inventory.empty (fill ~tid:1 ~side:Buy ~px ~sz:s ())
;;

let short s =
  Inventory.on_fill Inventory.empty (fill ~tid:1 ~side:Sell ~px:"100000" ~sz:s ())
;;

let check ?(now = 1_000) ?(peak = "0") market inventory =
  print_s
    [%sexp
      (Risk.check params ~now:(ms now) ~peak_pnl:(usd peak) market inventory : Risk.t)]
;;

let%expect_test "healthy" =
  check (market "100000") (long "0.005");
  [%expect {| Ok |}]
;;

let%expect_test "no data, stale data" =
  check (Market_state.create ~vol_window:10) Inventory.empty;
  [%expect {| (Halt No_market_data) |}];
  check ~now:5_001 (market "100000") Inventory.empty;
  [%expect {| (Halt (Stale_market_data (age_ms 5001))) |}]
;;

let%expect_test "position limit" =
  check (market "100000") (short "0.011");
  [%expect {| (Halt (Max_position (position -0.011))) |}]
;;

let%expect_test "drawdown from peak" =
  (* Long 0.01 from 100000; at 96000 the position is down 40 USD. *)
  check (market "96000") (long "0.01") ~peak:"5";
  [%expect {| Ok |}];
  check (market "96000") (long "0.01") ~peak:"15";
  [%expect {| (Halt (Max_drawdown (pnl -40) (peak 15))) |}];
  print_s [%sexp (Risk.Reason.is_sticky (Stale_market_data { age_ms = 1 }) : bool)];
  [%expect {| false |}]
;;

let%expect_test "flatten: cancel everything, reduce-only IOC through the mid" =
  let open_orders =
    [ open_order ~oid:1 ~side:Buy ~px:"99970" ~sz:"0.001" ()
    ; open_order ~oid:2 ~side:Sell ~px:"100030" ~sz:"0.001" ()
    ]
  in
  Risk.flatten params ~generation:3 ~open_orders (market "100000") (long "0.004")
  |> List.iter ~f:(fun a -> print_s [%sexp (a : Action.t)]);
  [%expect
    {|
    (Cancel 1)
    (Cancel 2)
    (Place
     ((symbol BTC) (side Sell) (size 0.004) (px (99500)) (order_type (Limit Ioc))
      (reduce_only true) (client_order_id (0x425443000000000001ff000000000003))))
    |}];
  Risk.flatten params ~generation:3 ~open_orders:[] (market "100000") (short "0.004")
  |> List.iter ~f:(fun a -> print_s [%sexp (a : Action.t)]);
  [%expect
    {|
    (Place
     ((symbol BTC) (side Buy) (size 0.004) (px (100500)) (order_type (Limit Ioc))
      (reduce_only true) (client_order_id (0x425443000000000000ff000000000003))))
    |}];
  (* Flat: only cancels. *)
  print_s
    [%sexp
      (Risk.flatten params ~generation:3 ~open_orders (market "100000") Inventory.empty
       : Action.t list)];
  [%expect {| ((Cancel 1) (Cancel 2)) |}]
;;

let%expect_test "action rate window" =
  let rate =
    List.fold
      [ 0, 6; 400, 12; 900, 0; 1_000, 3 ]
      ~init:Risk.Action_rate.empty
      ~f:(fun rate (at, actions) -> Risk.Action_rate.record rate ~now:(ms at) ~actions)
  in
  List.iter [ 1_000; 1_399; 1_400; 2_000 ] ~f:(fun now ->
    printf "%d ms: %d\n" now (Risk.Action_rate.last_second rate ~now:(ms now)));
  [%expect {|
    1000 ms: 15
    1399 ms: 15
    1400 ms: 3
    2000 ms: 0
    |}]
;;

let%expect_test "too many actions in a second trip a sticky halt" =
  let check actions =
    print_s
      [%sexp
        (Risk.check
           ~actions_last_second:actions
           params
           ~now:(ms 1_000)
           ~peak_pnl:Usd.zero
           (market "100000")
           Inventory.empty
         : Risk.t)]
  in
  check 100;
  [%expect {| Ok |}];
  check 101;
  [%expect {| (Halt (Max_action_rate (actions 101))) |}];
  print_s [%sexp (Risk.Reason.is_sticky (Max_action_rate { actions = 101 }) : bool)];
  [%expect {| true |}]
;;
