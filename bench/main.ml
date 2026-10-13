open! Core
open Exchange_core
open Mm_core
open Core_bench

(* The quoter on a two-sided book with a volatility estimate, flat and long. *)
let params = Params.btc

let market =
  Market_state.with_vol_bps
    (Market_state.on_book
       (Market_state.create ~vol_window:params.vol_window)
       { symbol = params.symbol
       ; time = 0
       ; bids = [ { px = Price.of_int 99_990; sz = Size.of_string "1.5"; n = 3 } ]
       ; asks = [ { px = Price.of_int 100_010; sz = Size.of_string "0.5"; n = 1 } ]
       })
    3.
;;

let long =
  Inventory.on_fill
    Inventory.empty
    { trade_id = Trade_id.of_int 1
    ; order_id = Order_id.of_int 1
    ; client_order_id = None
    ; symbol = params.symbol
    ; side = Buy
    ; px = Price.of_int 100_000
    ; size = Size.of_string "0.004"
    ; fee = Usd.zero
    ; is_maker = true
    ; time = 0
    ; closed_pnl = None
    }
;;

let resting =
  List.mapi (Quoter.quote params market long) ~f:(fun i (q : Quote.t) : Open_order.t ->
    { symbol = params.symbol
    ; side = q.side
    ; px = q.px
    ; size = q.sz
    ; orig_size = Some q.sz
    ; order_id = Order_id.of_int i
    ; client_order_id = None
    ; timestamp = 0
    })
;;

let next_book =
  { L2.symbol = params.symbol
  ; time = 1
  ; bids = [ { px = Price.of_int 99_991; sz = Size.of_string "1.2"; n = 2 } ]
  ; asks = [ { px = Price.of_int 100_011; sz = Size.of_string "0.7"; n = 2 } ]
  }
;;

(* A running strategy that has just quoted: the next tick inside the requote interval with
   an unchanged mid sends nothing. *)
let quoted =
  let s = Strategy.on_book (Strategy.create params) { next_book with time = 0 } in
  let s, plan = Strategy.tick s ~now:0 in
  let resting =
    List.filter_mapi plan ~f:(fun i -> function
      | Action.Place (r : Order_request.t) ->
        Some
          { Open_order.symbol = params.symbol
          ; side = r.side
          ; px = Option.value_exn r.px
          ; size = r.size
          ; orig_size = Some r.size
          ; order_id = Order_id.of_int i
          ; client_order_id = r.client_order_id
          ; timestamp = 0
          }
      | Keep _ | Cancel _ -> None)
  in
  Strategy.on_open_orders s resting
;;

let () =
  Command_unix.run
    (Bench.make_command
       [ Bench.Test.create ~name:"quote flat" (fun () ->
           ignore (Quoter.quote params market Inventory.empty : Quote.t list))
       ; Bench.Test.create ~name:"quote long" (fun () ->
           ignore (Quoter.quote params market long : Quote.t list))
       ; Bench.Test.create ~name:"quote+reconcile (keep all)" (fun () ->
           let desired = Quoter.quote params market long in
           ignore
             (Reconciler.reconcile
                params
                ~now:0
                ~generation:1
                ~open_orders:resting
                ~desired
              : Action.t list))
       ; Bench.Test.create ~name:"market_state on_book" (fun () ->
           ignore (Market_state.on_book market next_book : Market_state.t))
       ; Bench.Test.create ~name:"strategy tick (no change)" (fun () ->
           ignore (Strategy.tick quoted ~now:1 : Strategy.t * Action.t list))
       ; Bench.Test.create ~name:"strategy tick_u (no change)" (fun () ->
           let #(s, _) = Strategy.tick_u quoted ~now:1 in
           ignore (s : Strategy.t))
       ; Bench.Test.create ~name:"strategy on_book+tick (requote)" (fun () ->
           ignore
             (Strategy.tick (Strategy.on_book quoted next_book) ~now:10_000_000_000
              : Strategy.t * Action.t list))
       ])
;;
