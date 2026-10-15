(** The market maker as a deterministic state machine. Event handlers fold exchange data
    into the state; [tick] decides what to send. The live loop calls [tick] after events
    and on a timer; tests drive it directly.

    On each [tick ~now]:
    + PnL peak is updated at the current mid.
    + While a previous plan awaits an [on_open_orders] snapshot, nothing is sent for
      [requote_interval_ms]: placed orders have no oid yet, so a second plan could
      duplicate them. After that, plans may cancel and flatten but place no quotes until a
      snapshot arrives.
    + [Risk.check] picks the mode, counting the cancels and places of the plans sent in
      the last second against [max_actions_per_sec] (neither counted nor checked within
      [action_rate_grace_ms] of {!on_resync}). A sticky halt ([Max_position],
      [Max_drawdown], [Max_action_rate]) is permanent ([Halted]): every
      [requote_interval_ms] the flatten plan is sent until no orders rest and the position
      is flat. The one exception: with [action_rate_cooldown_ms] set, a [Max_action_rate]
      halt clears that long after it tripped, the action history is forgotten and the
      checks run again. Missing or stale data ([Pulled]) cancels all resting orders until
      fresh data arrives.
    + When the risk checks pass, the plausibility checks run: with
      [params.reference_guard], a venue mid outside the reference band past its grace
      period (or no fresh reference, when so configured) pulls quotes
      ([Pulled (Reference_divergence _ | Reference_missing _)]); with
      [params.toxicity_gate], a signal at its pull threshold pulls them
      ([Pulled (Toxic_flow _)]). A pull reason already in the mode is kept while it holds.
    + [Running] requotes when the last plan is older than [requote_interval_ms], the mid
      moved at least [requote_threshold_bps] since then, the position changed, the
      toxicity gate's decision changed, or the mode just became [Running]. The plan is
      [Reconciler.reconcile] of [Quoter.quote], with the gate's wider half-spread, smaller
      size and reservation shift while it widens.

    A returned plan is non-empty only when it contains a [Cancel] or [Place]; it then also
    lists the [Keep]s. Cancelled orders are dropped from the local open orders right away;
    the generation (part of every client order id) increases after each plan that places
    orders.

    Times ([now], book, order and fill timestamps) are nanoseconds since the epoch;
    intervals in {!Params} are [Time_ns.Span.t]. *)

open! Core
open Exchange_core

module Mode : sig
  type t =
    | Running
    | Pulled of Risk.Reason.t
    | Halted of Risk.Reason.t
  [@@deriving sexp, compare, equal]
end

type t [@@deriving sexp_of]

(** Raises if [Params.validate] fails. *)
val create : Params.t -> t

(** Events for other symbols are ignored. *)
val on_book : t -> L2.t -> t

val on_fill : t -> Fill.t -> t

(** A full snapshot of resting orders (other symbols are dropped). Ends the wait for an
    acknowledgement of the last plan. *)
val on_open_orders : t -> Open_order.t list -> t

(** Takes position and entry from an account snapshot ({!Inventory.sync}). *)
val on_account : t -> Account_view.t -> t

(** The venue connection was restored and the open orders are being re-read. For the next
    [action_rate_grace_ms] the plans sent are not counted towards [max_actions_per_sec]
    and the action rate is not checked, so the requote burst that follows cannot trip the
    kill switch. *)
val on_resync : t -> now:int -> t

(** A toxicity measurement at [now] for the {!Toxicity_gate} ([params.toxicity_gate]):
    [signal] (typically a VPIN percentile in [[0, 1]]) and the signed flow [imbalance] in
    [[-1, 1]]. Only the latest one counts. Without a gate it is kept but unused. *)
val on_toxicity : t -> now:int -> signal:float -> imbalance:float -> t

(** A reference mid at [now] for the {!Reference_guard} ([params.reference_guard]);
    ignored without one. *)
val on_reference : t -> now:int -> Price.t -> t

val tick : t -> now:int -> t * Action.t list

(** [tick] returning an unboxed pair. A tick that sends nothing and changes no field
    allocates nothing and returns its argument. *)
val tick_u : t -> now:int -> #(t * Action.t list)

(** Swaps the parameters of a running strategy, keeping its market state, inventory,
    resting orders, generation, mode and action history. The next [tick] requotes (unless
    a plan still awaits its snapshot), so the reconciler keeps the resting orders that
    still match and replaces the rest; nothing is cancelled wholesale. A halted strategy
    stays halted. [vol_window] changes restart the volatility estimate's smoothing from
    the current value.

    Errors, leaving [t] unchanged in the caller's hands, if [Params.validate] fails or the
    symbol differs. *)
val update_params : t -> Params.t -> t Or_error.t

val params : t -> Params.t
val mode : t -> Mode.t
val market : t -> Market_state.t
val inventory : t -> Inventory.t
val open_orders : t -> Open_order.t list
val generation : t -> int
val peak_pnl : t -> Usd.t

(** How far fills have taken the position past [soft_max_position], summed over the run
    (each fill counts only the part beyond both the soft limit and the position before
    it). The quoter gets [bump_budget - bump_used] as its [bump_left]. *)
val bump_used : t -> Size.t

(** The toxicity gate's decision at the last tick ([Normal] without a gate). *)
val toxicity_decision : t -> Toxicity_gate.Decision.t

(** [None] without [params.reference_guard]. *)
val reference_guard : t -> Reference_guard.t option

(** What the plausibility checks report. *)
module Event : sig
  type t =
    | Reference of Reference_guard.Event.t
    | Toxicity of Toxicity_gate.Event.t
  [@@deriving sexp, compare, equal]
end

(** The events between two states of one strategy (a tick, or an [on_reference] whose
    reference jumped): the guard's ({!Reference_guard.events_between}), then a change of
    the gate's decision kind. *)
val events_between : before:t -> after:t -> now:int -> Event.t list

(** [tick] and the events between its argument and its result. *)
val tick_with_events : t -> now:int -> t * Action.t list * Event.t list
