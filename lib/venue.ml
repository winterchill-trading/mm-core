open! Core
open! Async
open Exchange_core

module type S = sig
  type t
  type config

  val connect : config -> t Or_error.t Deferred.t
  val instrument : t -> Symbol.t -> Instrument.t Or_error.t Deferred.t
  val books : t -> Symbol.t -> L2.t Pipe.Reader.t Deferred.t
  val fills : t -> Fill.t Pipe.Reader.t
  val order_updates : t -> Order_update.t Pipe.Reader.t
  val open_orders : t -> Symbol.t -> Open_order.t list Or_error.t Deferred.t
  val account : t -> Account_view.t Or_error.t Deferred.t
  val place : t -> Order_request.t list -> Order_status.t list Or_error.t Deferred.t
  val cancel : t -> Symbol.t -> Order_id.t list -> unit Or_error.t Deferred.t
  val cancel_all : t -> Symbol.t -> unit Or_error.t Deferred.t
  val on_reconnect : t -> f:(unit -> unit) -> unit
  val close : t -> unit Deferred.t
end
