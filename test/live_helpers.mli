(** Helpers for tests of the Async live loop over {!Memory_venue}. *)

open! Core
open! Async
open Exchange_core

(** Polls [f] every 10ms; raises after [timeout] (default 10s). *)
val wait_until
  :  ?timeout:Time_ns.Span.t
  -> what:string
  -> (unit -> bool)
  -> unit Deferred.t

(** A one-level BTC book stamped with the current time. *)
val live_book : ?bid:string -> ?ask:string -> unit -> L2.t

(** The generation encoded in a client order id from [Mm_core.Reconciler.cloid]. *)
val generation_of_cloid : Client_order_id.t -> int

(** One line per order, best price first within each side, asks on top. *)
val print_orders : Open_order.t list -> unit
