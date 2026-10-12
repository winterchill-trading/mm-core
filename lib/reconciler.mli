(** Turns the desired ladder and the resting orders into a minimal plan.

    Each desired quote, in order, keeps the closest unused resting order of the same
    symbol and side that has exactly the desired size, is within [requote_threshold_bps]
    of the desired price and is younger than [order_ttl_ms]. A kept order must also stay
    strictly beyond the previous level's final price and strictly inside the next desired
    price on its side, so a ladder (each side listed in level order, as {!Quoter.quote}
    returns it) never has two orders on one price. Resting orders left over are cancelled;
    desired quotes left over are placed as post-only (ALO) limit orders.

    The plan lists cancels (in open-order order), then places (in desired order), then
    keeps. Resting orders on other symbols are ignored. *)

open! Core
open Exchange_core

(** Deterministic client order id: ["0x"] and 32 hex digits encoding 8 bytes of the symbol
    name (zero padded, truncated), 1 byte side (0 buy, 1 sell), 1 byte level and the low
    48 bits of the generation. [level] must be in [[0, 255]]. The 34 characters are
    accepted as a client id by Hyperliquid (128-bit cloid), Binance and Bybit (up to 36
    characters of [[0-9a-zA-Z_-]] plus a few symbols). *)
val cloid
  :  symbol:Symbol.t
  -> side:Side.t
  -> level:int
  -> generation:int
  -> Client_order_id.t

(** Level reserved for the reduce-only flatten order. *)
val flatten_level : int

(** [|current - desired| <= requote_threshold_bps] of [desired]. *)
val within_tolerance : Params.t -> current:Price.t -> desired:Price.t -> bool

(** The post-only order request for a desired quote. *)
val order_request : Params.t -> generation:int -> Quote.t -> Order_request.t

(** [now] is in nanoseconds since the epoch, like [Open_order.timestamp]. *)
val reconcile
  :  Params.t
  -> now:int
  -> generation:int
  -> open_orders:Open_order.t list
  -> desired:Quote.t list
  -> Action.t list
