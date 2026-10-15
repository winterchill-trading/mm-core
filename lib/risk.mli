(** Kill-switch checks and the flatten plan.

    Checks, in order of precedence:
    - [Max_position]: [|position| > max_position]
    - [Max_drawdown]: [peak_pnl - pnl > max_drawdown], with
      [pnl = realized + unrealized(mid) - fees] (skipped without a mid)
    - [Max_action_rate]: more than [max_actions_per_sec] cancels and places sent in the
      last second (only when the caller passes [actions_last_second])
    - [No_market_data]: no two-sided book yet
    - [Stale_market_data]: [now - book time > max_data_age_ms]

    [Max_position], [Max_drawdown] and [Max_action_rate] are sticky: the strategy stays
    halted and flattens. Missing or stale data only pulls quotes until fresh data arrives.
    {!check} never returns the reference and toxicity reasons; {!Strategy} pulls with them
    when {!Reference_guard} or {!Toxicity_gate} says so, after these checks pass. *)

open! Core
open Exchange_core

module Reason : sig
  type t =
    | Max_position of { position : Size.t }
    | Max_drawdown of
        { pnl : Usd.t
        ; peak : Usd.t
        }
    | Max_action_rate of { actions : int }
    (** a runaway loop: [actions] cancels and places in the last second *)
    | No_market_data
    | Stale_market_data of { age_ms : int }
    | Reference_divergence of
        { divergence_bps : float
        ; held_ms : int
        }
    (** the venue mid has been outside the reference band for [held_ms]
        ({!Reference_guard}) *)
    | Reference_missing of { age_ms : int option }
    (** a reference guard set to pull without a fresh reference *)
    | Toxic_flow of { signal : float } (** the toxicity gate's pull threshold *)
  [@@deriving sexp, compare, equal]

  val is_sticky : t -> bool
end

(** Cancels and places sent, over a rolling one second window: a queue of (time, count)
    entries with their running sum. Recording drops expired entries from the front, so
    both operations are amortised O(1) however many plans a second brings. Times must not
    go backwards. *)
module Action_rate : sig
  type t [@@deriving sexp_of]

  val empty : t

  (** Records [actions] sent at [now] (nanoseconds since the epoch). *)
  val record : t -> now:int -> actions:int -> t

  (** Actions recorded in [(now - 1s, now]]. *)
  val last_second : t -> now:int -> int
end

type t =
  | Ok
  | Halt of Reason.t
[@@deriving sexp, compare, equal]

(** [now] in nanoseconds since the epoch. *)
val check
  :  ?actions_last_second:int
  -> Params.t
  -> now:int
  -> peak_pnl:Usd.t
  -> Market_state.t
  -> Inventory.t
  -> t

(** [check] with the action count as [This n] or [Null] (not checked). Allocates nothing
    when the result is [Ok]. *)
val check_or_null
  :  actions_last_second:int or_null
  -> Params.t
  -> now:int
  -> peak_pnl:Usd.t
  -> Market_state.t
  -> Inventory.t
  -> t

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
