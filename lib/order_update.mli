(** A change in the lifecycle of one of the account's orders, as a venue client reports
    it. *)

open! Core
open Exchange_core

module Status : sig
  type t =
    | New (** accepted and resting, nothing filled *)
    | Partially_filled (** resting, part of it filled *)
    | Filled (** completely filled; off the book *)
    | Cancelled (** cancelled by the account, the venue or self-trade prevention *)
    | Rejected (** never rested *)
  [@@deriving sexp, compare, equal, enumerate]

  (** [Filled], [Cancelled] and [Rejected]: the order is no longer on the book. *)
  val is_final : t -> bool
end

type t =
  { order_id : Order_id.t
  ; client_order_id : Client_order_id.t option
  ; symbol : Symbol.t
  ; side : Side.t
  ; px : Price.t (** limit price *)
  ; status : Status.t
  ; filled : Size.t (** cumulative filled size *)
  ; remaining : Size.t (** size still resting; zero once final *)
  ; time : int (** nanoseconds since the epoch, from the venue's clock *)
  }
[@@deriving sexp, compare, equal]
