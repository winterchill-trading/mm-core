(** The live event loop: connects {!Strategy} to a venue through a {!Venue.S} client.

    - [Venue.books] and [Venue.fills] feed [Strategy.on_book] and [Strategy.on_fill].
    - [Venue.open_orders] and [Venue.account] are polled every [requote_interval_ms],
      after every executed plan (the strategy waits for that snapshot before placing
      again), when an order update reports an order leaving the book, and after a
      reconnect. A reconnect also starts the action rate grace period
      ({!Strategy.on_resync}).
    - [Strategy.tick] runs after every book update, fill and poll. A non-empty plan is
      sent as at most two venue calls: one [cancel] with every cancel, then one [place]
      with every place.

    Ticks, plan execution and polls run one at a time on a sequencer, so a snapshot always
    reflects the plans sent before it. Market data and fills are applied as they arrive. *)

open! Core
open! Async
open Exchange_core

module Report : sig
  (** Garbage collector counters of the process, from [Gc.quick_stat] (cheap: no heap
      walk). Collections are counted since the loop started; heap sizes are current. *)
  module Gc_counts : sig
    type t =
      { minor_collections : int
      ; major_collections : int
      ; heap_words : int (** major heap size *)
      }
    [@@deriving sexp_of]
  end

  type t =
    { elapsed : Time_ns.Span.t
    ; fills : int
    ; maker_fills : int
    ; volume : Usd.t (** sum of [px * size] over our fills *)
    ; realized_pnl : Usd.t
    ; unrealized_pnl : Usd.t (** at the last mid *)
    ; fees : Usd.t
    ; total_pnl : Usd.t (** realized + unrealized - fees *)
    ; position : Size.t
    ; mid : Price.t option
    ; account_value : Usd.t option (** equity from the last account snapshot *)
    ; plans : int
    ; orders_placed : int (** accepted: resting or filled *)
    ; orders_rejected : int
    ; rejections : (string * int) list (** reject reason and count *)
    ; orders_cancelled : int (** cancellations reported by order updates *)
    ; reconnects : int
    ; mode : Strategy.Mode.t
    ; gc : Gc_counts.t
    }
  [@@deriving sexp_of]

  (** A multi-line, human readable summary. *)
  val to_string_hum : t -> string
end

module Config : sig
  type t =
    { params : Params.t
    ; summary_interval : Time_ns.Span.t
    (** how often position, PnL and resting quotes are logged *)
    }
  [@@deriving sexp_of]
end

(** One line for a rejection, as counted in [Report.rejections]. *)
val describe_reject : Reject_reason.t -> string

(** Becomes determined on the first SIGINT or SIGTERM, or after [run_for] (measured on
    [time_source], default the wall clock). A second signal logs and exits the process
    with code 130 at once. Call it once per process. *)
val stop_signal
  :  ?time_source:Time_source.t
  -> ?run_for:Time_ns.Span.t
  -> unit
  -> unit Deferred.t

(** Every function taking [?time_source] reads the time and runs its timers (polls,
    summaries, the shutdown grace period, timeouts) on it; the default is the wall clock.
    A synthetic [Time_source.create] makes a run deterministic in tests. *)
module Make (V : Venue.S) : sig
  type t

  (** Takes an initial open orders and account snapshot, subscribes to the book and
      account feeds of [venue] and starts quoting. The loop owns [venue] from here on:
      [stop] closes it. *)
  val start : ?time_source:Time_source.t -> V.t -> Config.t -> t Or_error.t Deferred.t

  (** Stops quoting: waits for the step in flight, cancels every resting order on the
      symbol (re-reading open orders until none are left, up to a few attempts), takes a
      final account snapshot, closes the venue and returns the report. Idempotent. *)
  val stop : t -> Report.t Deferred.t

  (** Swaps the strategy parameters of the running loop ({!Strategy.update_params}): no
      restart and no cancel-all. The next tick requotes with the new parameters, the
      reconciler keeping the resting orders that still match; a changed
      [requote_interval_ms] re-arms the poll timer. Errors if the parameters are invalid,
      name another symbol, or the loop is stopping. *)
  val update_params : t -> Params.t -> unit Or_error.t

  (** Running totals so far; [stop] returns the final one. *)
  val report : t -> Report.t

  val strategy : t -> Strategy.t

  (** Registers [f], called on every venue reconnect right after the loop's own strategy
      has started its action rate grace period ({!Strategy.on_resync}) and before the
      resync poll is queued, in registration order. Observers that mirror the strategy (a
      shadow strategy, a strategy log) call [Strategy.on_resync] on their copy here so it
      starts the same grace period at the same time. *)
  val on_resync : t -> f:(unit -> unit) -> unit

  (** Gives the strategy a reference mid ({!Strategy.on_reference}, for
      [params.reference_guard]) at the time source's now, and ticks. A reference jump is
      reported as an event at once. Ignored once the loop is stopping. *)
  val on_reference : t -> Price.t -> unit

  (** Gives the strategy a toxicity measurement ({!Strategy.on_toxicity}, for
      [params.toxicity_gate]) at the time source's now, and ticks. *)
  val on_toxicity : t -> signal:float -> imbalance:float -> unit

  (** Registers [f], called with every plausibility event ({!Strategy.Event.t}: the
      reference guard diverging, resuming, losing its reference or seeing a jump; the
      toxicity gate widening, pulling or clearing), in registration order. Each event is
      also logged. Nothing is computed without a gate or guard in the parameters. *)
  val on_strategy_event : t -> f:(Strategy.Event.t -> unit) -> unit

  (** Tick-to-order latency: from the arrival of a book update (on the time source) to the
      moment the plan it led to is sent, for every plan sent after a book update. The
      clock starts at the first book since the previous tick, so time spent waiting for
      the sequencer (a plan or poll in flight) counts. [None] before the first such plan. *)
  val tick_to_order : t -> Latency_histogram.Stats.t option

  (** The mid of the first two-sided book of [symbol] (default timeout 10s). *)
  val first_mid
    :  ?time_source:Time_source.t
    -> ?timeout:Time_ns.Span.t
    -> V.t
    -> Symbol.t
    -> Price.t Or_error.t Deferred.t

  (** [start], wait for [stop] (a deferred), then [stop]. *)
  val run
    :  ?time_source:Time_source.t
    -> V.t
    -> Config.t
    -> stop:unit Deferred.t
    -> Report.t Or_error.t Deferred.t

  (** Connects, fetches the instrument and its first mid, builds the parameters with
      [params], then {!start}. Returns the running loop: a handle for {!update_params},
      {!report}, {!tick_to_order} and {!stop}. Closes the venue on every error path.

      [on_venue] runs once, right after the connection and before the instrument and first
      mid are fetched, so observers can attach to the venue (subscribe to its feeds,
      register [on_reconnect] hooks) before the loop starts. An exception it raises
      becomes the returned error. *)
  val connect_and_start
    :  ?time_source:Time_source.t
    -> ?on_venue:(V.t -> unit)
    -> V.config
    -> symbol:Symbol.t
    -> params:(Instrument.t -> mid:Price.t -> Params.t Or_error.t)
    -> summary_interval:Time_ns.Span.t
    -> t Or_error.t Deferred.t

  (** {!connect_and_start}, wait for [stop], then {!stop}. Closes the venue on every path. *)
  val connect_and_run
    :  ?time_source:Time_source.t
    -> ?on_venue:(V.t -> unit)
    -> V.config
    -> symbol:Symbol.t
    -> params:(Instrument.t -> mid:Price.t -> Params.t Or_error.t)
    -> summary_interval:Time_ns.Span.t
    -> stop:unit Deferred.t
    -> Report.t Or_error.t Deferred.t
end
