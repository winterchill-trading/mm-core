(** Kill-switch checks and the flatten plan.

    Checks, in order of precedence:
    - [Max_position]: [|position| > max_position]
    - [Max_drawdown]: [peak_pnl - pnl > max_drawdown], with
      [pnl = realized + unrealized(mid) - fees] (skipped without a mid)
    - [No_market_data]: no two-sided book yet
    - [Stale_market_data]: [now - book time > max_data_age_ms]

    [Max_position] and [Max_drawdown] are sticky: the strategy stays halted and flattens.
    Missing or stale data only pulls quotes until fresh data arrives. *)

open! Core
open Exchange_core

module Reason : sig
  type t =
    | Max_position of { position : Size.t }
    | Max_drawdown of
        { pnl : Usd.t
        ; peak : Usd.t
        }
    | No_market_data
    | Stale_market_data of { age_ms : int }
  [@@deriving sexp, compare, equal]

  val is_sticky : t -> bool
end

type t =
  | Ok
  | Halt of Reason.t
[@@deriving sexp, compare, equal]

(** [now] in nanoseconds since the epoch. *)
val check : Params.t -> now:int -> peak_pnl:Usd.t -> Market_state.t -> Inventory.t -> t

(** Cancels every resting order on the symbol and, when there is a position and a mid,
    sends a reduce-only IOC for the whole position at [mid -/+ flatten_slippage_bps]
    (rounded away from the mid to the tick), with client id level
    [Reconciler.flatten_level]. *)
val flatten
  :  Params.t
  -> generation:int
  -> open_orders:Open_order.t list
  -> Market_state.t
  -> Inventory.t
  -> Action.t list

(** Only the cancels of [flatten]. *)
val cancel_all : Params.t -> open_orders:Open_order.t list -> Action.t list
