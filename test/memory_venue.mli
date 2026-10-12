(** An in-memory {!Mm_core.Venue.S} for live loop tests: rests every limit order
    (rejecting post-only orders that would cross the last pushed book), cancels on request
    and lets the test push books and fills. Pushed books do not see our orders. The
    account reports the net position of the distinct fills pushed so far, entered at the
    first fill's price.

    With {!Bugs} it misbehaves the way real venues do, driven by a seeded random state so
    a run is repeatable for a given sequence of calls. *)

open! Core
open! Async
open Exchange_core
open Mm_core

module Bugs : sig
  type t =
    { seed : int
    ; reject_percent : int (** each placed order is rejected with this probability *)
    ; cancel_ack_delay : Time_ns.Span.t
    (** cancels succeed at once, but orders leave the book (and their [Cancelled] update
        is sent) only after this delay *)
    ; split_fills : int (** a pushed fill arrives as 1 to [split_fills] partial fills *)
    ; stale_poll_percent : int
    (** [open_orders] returns the previous snapshot with this probability *)
    ; late_places : int
    (** the first [late_places] calls to [place] fail with a timeout, yet their orders
        rest on the book [late_place_delay] later *)
    ; late_place_delay : Time_ns.Span.t
    ; place_latency : Time_ns.Span.t (** every [place] call takes this long to answer *)
    }
  [@@deriving sexp_of]

  (** A well-behaved venue. *)
  val none : t
end

(** Counts of the bugs actually injected. *)
module Injected : sig
  type t =
    { rejects : int
    ; late_cancels : int
    ; split_fills : int (** fills delivered in more than one part *)
    ; stale_polls : int
    ; late_places : int
    }
  [@@deriving sexp_of]
end

include Venue.S with type config = Bugs.t

(** [connect bugs] is [create bugs] on the wall clock. With a synthetic [time_source], bug
    delays and order timestamps follow it. *)
val create : ?time_source:Time_source.t -> Bugs.t -> t

val push_book : t -> L2.t -> unit

(** Delivers a fill. Under {!Bugs} it may arrive in parts, with trade ids
    [tid * 100 + part] (a fill delivered whole keeps its trade id). *)
val push_fill : t -> Fill.t -> unit

(** Fills a resting order completely as a maker: it leaves the book with a [Filled] update
    and its fill is pushed (see {!push_fill}) with trade id [tid]. *)
val fill_resting : t -> Open_order.t -> tid:int -> unit

(** Resting orders, in placement order. *)
val orders : t -> Open_order.t list

(** Net position of the distinct fills delivered so far. *)
val position : t -> Size.t

val injected : t -> Injected.t
val is_closed : t -> bool
