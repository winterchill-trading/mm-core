(** One step of an order plan. The executor sends [Cancel] and [Place] to the exchange
    (cancels before places) and ignores [Keep], which records that a resting order already
    matches a desired quote. *)

open! Core
open Exchange_core

type t =
  | Keep of Order_id.t
  | Cancel of Order_id.t
  | Place of Order_request.t
[@@deriving sexp, compare, equal]

(** [true] for [Cancel] and [Place]. *)
val is_change : t -> bool
