(** A plausibility check on the venue's prices: quoting stops when the venue mid strays
    from an independent reference mid (another venue's mid, an index, a price-anchor mark)
    by more than [band_bps] for longer than [grace_ms].

    {!Risk} already pulls quotes on missing or stale market data; this guard catches data
    that is fresh but wrong, such as a venue book that kept updating from a broken feed,
    or a reference the quotes were priced off that froze while the market moved. The guard
    is pure: {!on_reference} records a reference price, {!check} looks at the venue mid at
    [now], and {!status} says whether to pull.

    - Divergence is [10_000 * (venue_mid - reference) / reference] bps.
    - Inside the band the status is [Within_band]. Outside it, [Diverging] until it has
      stayed outside for [grace_ms], then [Diverged] (pull) until it is back inside.
    - With [max_jump_bps], a reference update that moves more than that from the previous
      reference skips the grace period of the divergence it causes: a jump in the
      reference is itself evidence that one side is wrong.
    - Without a reference, or with one older than [max_reference_age_ms],
      [stale_reference] decides: [Pull] gives [Reference_missing] (pull), [Ignore] gives
      [Unchecked] (quote on). Without a venue mid the status is [Unchecked]; {!Risk}
      handles that case.

    Times are nanoseconds since the epoch. *)

open! Core
open Exchange_core

module Stale_reference : sig
  type t =
    | Pull
    | Ignore
  [@@deriving sexp, compare, equal]
end

module Config : sig
  type t =
    { band_bps : float
    ; grace_ms : Time_ns.Span.t
    ; max_jump_bps : float option
    ; max_reference_age_ms : Time_ns.Span.t
    ; stale_reference : Stale_reference.t
    }
  [@@deriving sexp, compare, equal]

  (** 50 bps for 2 s, no jump check, references valid for 30 s, [Pull] without one. *)
  val default : t

  (** Positive finite band and jump, non-negative grace, positive age. *)
  val validate : t -> t Or_error.t
end

module Status : sig
  type t =
    | Unchecked
    | Within_band
    | Diverging of
        { since : int
        ; divergence_bps : float
        }
    | Diverged of
        { since : int
        ; divergence_bps : float
        ; venue_mid : Price.t
        ; reference_mid : Price.t
        }
    | Reference_missing of { age_ms : int option (** [None]: no reference yet *) }
  [@@deriving sexp, compare, equal]

  (** [Diverged] and [Reference_missing]. *)
  val pulls : t -> bool
end

type t [@@deriving sexp_of]

val create : Config.t -> t
val config : t -> Config.t

(** Records a reference mid seen at [now]. *)
val on_reference : t -> now:int -> Price.t -> t

(** Re-evaluates the status against the venue mid at [now]. Returns its argument, and
    allocates nothing, when the status stays [Unchecked] or [Within_band]. *)
val check : t -> now:int -> venue_mid:Price.t or_null -> t

val status : t -> Status.t
val reference : t -> Price.t option

(** Swaps the configuration, keeping the reference and the status (re-evaluated at the
    next {!check}). *)
val with_config : t -> Config.t -> t

module Event : sig
  type t =
    | Diverged of
        { divergence_bps : float
        ; held_ms : int (** how long it had been outside the band *)
        ; venue_mid : Price.t
        ; reference_mid : Price.t
        }
    | Reference_jumped of
        { from_ : Price.t
        ; to_ : Price.t
        ; jump_bps : float
        }
    | Reference_missing of { age_ms : int option }
    | Resumed (** no longer pulling *)
  [@@deriving sexp, compare, equal]
end

(** What changed between two states of one guard: a jump recorded in between, then the
    move into or out of a pulling status. *)
val events_between : before:t -> after:t -> now:int -> Event.t list
