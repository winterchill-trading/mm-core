(** An in-memory {!Mm_core.Venue.S} for live loop tests: rests every limit order, cancels
    on request and lets the test push books and fills. The account reports the position of
    the first pushed fill. *)

open! Core
open! Async
open Exchange_core
open Mm_core
include Venue.S with type config = unit

val push_book : t -> L2.t -> unit
val push_fill : t -> Fill.t -> unit

(** Resting orders, in placement order. *)
val orders : t -> Open_order.t list

val is_closed : t -> bool
