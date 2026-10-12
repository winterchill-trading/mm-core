(** Test fixtures. Times are given in milliseconds and stored in nanoseconds. *)

open! Core
open Exchange_core

(** Milliseconds to nanoseconds. *)
val ms : int -> int

val px : string -> Price.t
val sz : string -> Size.t
val usd : string -> Usd.t
val btc : Symbol.t

val book
  :  ?time:int
  -> ?bid_sz:string
  -> ?ask_sz:string
  -> bid:string
  -> ask:string
  -> unit
  -> L2.t

val fill
  :  ?time:int
  -> ?fee:string
  -> ?closed_pnl:string
  -> tid:int
  -> side:Side.t
  -> px:string
  -> sz:string
  -> unit
  -> Mm_core.Fill.t

val open_order
  :  ?timestamp:int
  -> ?client_order_id:Client_order_id.t
  -> oid:int
  -> side:Side.t
  -> px:string
  -> sz:string
  -> unit
  -> Open_order.t
