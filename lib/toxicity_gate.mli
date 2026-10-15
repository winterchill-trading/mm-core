(** How the strategy answers toxic order flow: a pure function from a toxicity signal to a
    quoting adjustment.

    The signal is a number the caller computes from the trades it sees, typically the
    percentile of a volume-synchronised probability of informed trading (VPIN) against its
    own history, in [[0, 1]]; market-data's [Toxicity] module produces one per filled
    volume bucket, with the signed order imbalance of that bucket. mm-core does not depend
    on market-data: any producer works.

    With [i = clamp ((signal - widen_from) / (pull_at - widen_from), 0, 1)]:

    - [signal < widen_from]: [Normal], quotes unchanged;
    - [widen_from <= signal < pull_at]: [Widen], the half-spread grows by
      [i * max_extra_half_spread_bps], the reservation price moves by
      [i * skew_bps * imbalance] bps (towards the side the flow comes from: up when takers
      buy, so the quotes informed buyers would hit move away), and order sizes shrink by
      the factor [1 - i * size_cut];
    - [signal >= pull_at]: [Pull], no quotes on either side.

    A signal older than [max_signal_age_ms] counts as no signal ([Normal]): the gate
    adjusts quoting while toxicity is measured, it does not stop quoting when the producer
    goes quiet; stale market data has its own check in {!Risk}. *)

open! Core

module Config : sig
  type t =
    { widen_from : float (** signal where widening starts *)
    ; pull_at : float (** signal at or above which quotes are pulled *)
    ; max_extra_half_spread_bps : float (** extra half-spread just below [pull_at] *)
    ; skew_bps : float (** reservation shift at full intensity and imbalance 1 *)
    ; size_cut : float (** share of [order_size] removed at full intensity, in [[0, 1)] *)
    ; max_signal_age_ms : Time_ns.Span.t
    }
  [@@deriving sexp, compare, equal]

  (** Widen from 0.8, pull at 0.95, up to 5 bps more half-spread, 2 bps of skew, sizes cut
      by up to a half, signals valid for 60 s. *)
  val default : t

  (** [widen_from < pull_at], non-negative bps, [0 <= size_cut < 1], a positive age, all
      floats finite. *)
  val validate : t -> t Or_error.t
end

module Signal : sig
  type t =
    { value : float
    ; imbalance : float (** signed flow, [(buy - sell) / volume], in [[-1, 1]] *)
    ; time : int (** when it was measured, nanoseconds since the epoch *)
    }
  [@@deriving sexp, compare, equal]
end

module Decision : sig
  type t =
    | Normal
    | Widen of
        { intensity : float (** [i] above *)
        ; extra_half_spread_bps : float
        ; reservation_shift_bps : float
        ; size_factor : float
        }
    | Pull of { signal : float }
  [@@deriving sexp, compare, equal]
end

(** The decision for the latest signal at [now]. *)
val decide : Config.t -> now:int -> Signal.t option -> Decision.t

(** A change of decision kind, for logs and alerts. *)
module Event : sig
  type t =
    | Widening of { signal : float }
    | Pulled of { signal : float }
    | Cleared (** back to [Normal]: the signal fell below [widen_from] or went stale *)
  [@@deriving sexp, compare, equal]

  (** The event of going from [before] to [after]; none when the kind is unchanged
      ([Widen] to [Widen] with another intensity is not an event). *)
  val of_change : before:Decision.t -> after:Decision.t -> signal:float -> t option
end
