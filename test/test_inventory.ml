open! Core
open Exchange_core
open Mm_core
open Helpers

let show ?(mark = "100000") inv =
  print_s
    [%message
      ""
        ~position:(Inventory.position inv : Size.t)
        ~entry:(Inventory.avg_entry inv : Price.t option)
        ~realized:(Inventory.realized_pnl inv : Usd.t)
        ~fees:(Inventory.fees inv : Usd.t)
        ~unrealized:(Inventory.unrealized_pnl inv ~mark:(px mark) : Usd.t)
        ~total:(Inventory.total_pnl inv ~mark:(px mark) : Usd.t)]
;;

let%expect_test "average cost, partial close, flip" =
  let inv = Inventory.empty in
  let inv =
    Inventory.on_fill inv (fill ~tid:1 ~side:Buy ~px:"100000" ~sz:"0.01" ~fee:"0.1" ())
  in
  let inv =
    Inventory.on_fill inv (fill ~tid:2 ~side:Buy ~px:"100100" ~sz:"0.01" ~fee:"0.1" ())
  in
  show inv;
  [%expect
    {|
    ((position 0.02) (entry (100050)) (realized 0) (fees 0.2) (unrealized -1)
     (total -1.2))
    |}];
  (* Duplicate tid is ignored. *)
  let inv =
    Inventory.on_fill inv (fill ~tid:2 ~side:Buy ~px:"100100" ~sz:"0.01" ~fee:"0.1" ())
  in
  print_s [%sexp (Inventory.position inv : Size.t)];
  [%expect {| 0.02 |}];
  let inv = Inventory.on_fill inv (fill ~tid:3 ~side:Sell ~px:"100200" ~sz:"0.005" ()) in
  show inv;
  [%expect
    {|
    ((position 0.015) (entry (100050)) (realized 0.75) (fees 0.2)
     (unrealized -0.75) (total -0.2))
    |}];
  (* Sell 0.025: closes the remaining 0.015 long, opens 0.01 short at 100150. *)
  let inv = Inventory.on_fill inv (fill ~tid:4 ~side:Sell ~px:"100150" ~sz:"0.025" ()) in
  show inv ~mark:"100050";
  [%expect
    {|
    ((position -0.01) (entry (100150)) (realized 2.25) (fees 0.2) (unrealized 1)
     (total 3.05))
    |}];
  let inv = Inventory.on_fill inv (fill ~tid:5 ~side:Buy ~px:"100000" ~sz:"0.01" ()) in
  show inv;
  [%expect
    {|
    ((position 0) (entry ()) (realized 3.75) (fees 0.2) (unrealized 0)
     (total 3.55))
    |}]
;;

let%expect_test "account sync overrides position; older fills only add fees" =
  let state : Account_view.t =
    { account = Account.of_string "mm"
    ; balance = usd "1000"
    ; equity = usd "1000"
    ; total_notional = usd "300"
    ; margin_used = usd "15"
    ; maintenance_margin = usd "0"
    ; available = usd "985"
    ; positions =
        [ { symbol = btc
          ; size = sz "-0.003"
          ; entry_px = px "99000"
          ; notional = usd "300"
          ; unrealized_pnl = usd "0"
          ; realized_pnl = usd "0"
          ; cum_funding = usd "0"
          ; liquidation_px = None
          ; leverage = 20
          ; max_leverage = 40
          ; margin_used = usd "15"
          }
        ]
    ; time = ms 1_000
    }
  in
  let inv = Inventory.sync Inventory.empty state ~symbol:btc in
  let inv =
    Inventory.on_fill
      inv
      (fill
         ~time:900
         ~tid:1
         ~side:Sell
         ~px:"99000"
         ~sz:"0.003"
         ~fee:"0.05"
         ~closed_pnl:"1.5"
         ())
  in
  show inv ~mark:"98000";
  [%expect
    {|
    ((position -0.003) (entry (99000)) (realized 1.5) (fees 0.05) (unrealized 3)
     (total 4.45))
    |}];
  let inv =
    Inventory.on_fill inv (fill ~time:1_100 ~tid:2 ~side:Buy ~px:"98000" ~sz:"0.003" ())
  in
  show inv ~mark:"98000";
  [%expect
    {|
    ((position 0) (entry ()) (realized 4.5) (fees 0.05) (unrealized 0)
     (total 4.45))
    |}]
;;
