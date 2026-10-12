open! Core
open Exchange_core

type t =
  { trade_id : Trade_id.t
  ; order_id : Order_id.t
  ; client_order_id : Client_order_id.t option
  ; symbol : Symbol.t
  ; side : Side.t
  ; px : Price.t
  ; size : Size.t
  ; fee : Usd.t
  ; is_maker : bool
  ; time : int
  ; closed_pnl : Usd.t option
  }
[@@deriving sexp, compare, equal]
