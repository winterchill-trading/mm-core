(** One of the account's fills, as a venue client reports it. *)

open! Core
open Exchange_core

type t =
  { trade_id : Trade_id.t (** unique per venue; fills are de-duplicated by it *)
  ; order_id : Order_id.t
  ; client_order_id : Client_order_id.t option
  ; symbol : Symbol.t
  ; side : Side.t (** the account's side *)
  ; px : Price.t
  ; size : Size.t (** positive *)
  ; fee : Usd.t (** paid by the account; negative for a rebate *)
  ; is_maker : bool
  ; time : int (** nanoseconds since the epoch, from the venue's clock *)
  ; closed_pnl : Usd.t option
  (** PnL the venue realised on this fill, before fees, if it reports one *)
  }
[@@deriving sexp, compare, equal]
