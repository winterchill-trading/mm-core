open! Core
open Exchange_core

let ms n = n * 1_000_000
let px = Price.of_string
let sz = Size.of_string
let usd = Usd.of_string
let btc = Symbol.of_string "BTC"

let book ?(time = 0) ?(bid_sz = "1") ?(ask_sz = "1") ~bid ~ask () : L2.t =
  { symbol = btc
  ; time = ms time
  ; bids = [ { px = px bid; sz = sz bid_sz; n = 1 } ]
  ; asks = [ { px = px ask; sz = sz ask_sz; n = 1 } ]
  }
;;

let fill ?(time = 0) ?(fee = "0") ?(closed_pnl = "0") ~tid ~side ~px:p ~sz:s ()
  : Mm_core.Fill.t
  =
  { trade_id = Trade_id.of_int tid
  ; order_id = Order_id.of_int tid
  ; client_order_id = None
  ; symbol = btc
  ; side
  ; px = px p
  ; size = sz s
  ; fee = usd fee
  ; is_maker = true
  ; time = ms time
  ; closed_pnl = Some (usd closed_pnl)
  }
;;

let open_order ?(timestamp = 0) ?client_order_id ~oid ~side ~px:p ~sz:s () : Open_order.t =
  { symbol = btc
  ; side
  ; px = px p
  ; size = sz s
  ; orig_size = None
  ; order_id = Order_id.of_int oid
  ; client_order_id
  ; timestamp = ms timestamp
  }
;;
