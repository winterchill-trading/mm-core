(** What the live loop needs from an exchange: an Async client signature that a venue
    adapter (Hyperliquid, Binance, Bybit, a simulator) implements over its own wire
    protocol, converting to the [exchange-core] types.

    Conventions every implementation follows:

    - Timestamps are nanoseconds since the epoch ([L2.t.time], [Open_order.t.timestamp],
      [Fill.t.time], [Account_view.t.time]), converted from the venue's unit.
    - Sizes are positive, in base units; positions are signed.
    - Calls that reach the network return [Or_error.t]: transport failures, HTTP errors
      and rejected requests become [Error]. A request that the venue accepted but whose
      orders it rejected one by one is [Ok], with [Order_status.Rejected] per order.
    - {b Fills are de-duplicated by trade id.} [fills] delivers each trade id at most once
      per connection, including across reconnects and the snapshots some venues resend on
      resubscription, and never delivers fills that happened before [connect]. Consumers
      may still de-duplicate defensively.
    - Market data and account feeds survive reconnects: the implementation reconnects and
      resubscribes by itself, then calls the [on_reconnect] hooks so the consumer can
      resynchronise anything it may have missed (open orders, account). *)

open! Core
open! Async
open Exchange_core

module type S = sig
  (** A connected client acting for one account. *)
  type t

  (** Venue-specific connection settings: endpoints, account, credentials. *)
  type config

  (** Connects (and authenticates, where the venue needs it) and starts the account feeds,
      so [fills] and [order_updates] buffer from now on. Fails if the first connection
      fails. *)
  val connect : config -> t Or_error.t Deferred.t

  (** Trading rules of a symbol. [price_tick] is the increment valid at the current price,
      for venues whose tick depends on the price. *)
  val instrument : t -> Symbol.t -> Instrument.t Or_error.t Deferred.t

  (** Subscribes to book snapshots of [symbol]: each call returns a fresh reader that
      receives every snapshot from then on, best price first. Closing the reader stops
      delivery to it. *)
  val books : t -> Symbol.t -> L2.t Pipe.Reader.t Deferred.t

  (** The account's fills on every symbol, each trade id at most once (see above). One
      reader per connection, buffered from [connect]; read it from a single consumer. *)
  val fills : t -> Fill.t Pipe.Reader.t

  (** Lifecycle changes of the account's orders. One reader per connection, buffered from
      [connect]; read it from a single consumer. Venues that do not report partial fills
      as updates may skip [Partially_filled]. *)
  val order_updates : t -> Order_update.t Pipe.Reader.t

  (** The account's resting orders on [symbol]. *)
  val open_orders : t -> Symbol.t -> Open_order.t list Or_error.t Deferred.t

  (** Balance, equity and positions of the account. *)
  val account : t -> Account_view.t Or_error.t Deferred.t

  (** Places orders, in as few requests as the venue allows. The result has one status per
      request, in order. *)
  val place : t -> Order_request.t list -> Order_status.t list Or_error.t Deferred.t

  (** Cancels orders on [symbol] by exchange id. Orders that are already gone (filled,
      cancelled) are not an error. *)
  val cancel : t -> Symbol.t -> Order_id.t list -> unit Or_error.t Deferred.t

  (** Cancels every resting order of the account on [symbol]. *)
  val cancel_all : t -> Symbol.t -> unit Or_error.t Deferred.t

  (** Registers [f], called after every successful reconnection once subscriptions are
      restored (not after the first connection). *)
  val on_reconnect : t -> f:(unit -> unit) -> unit

  (** Closes connections and every pipe handed out. Idempotent. *)
  val close : t -> unit Deferred.t
end
