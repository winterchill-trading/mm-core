(** The live event loop: connects {!Strategy} to a venue through a {!Venue.S} client.

    - [Venue.books] and [Venue.fills] feed [Strategy.on_book] and [Strategy.on_fill].
    - [Venue.open_orders] and [Venue.account] are polled every [requote_interval_ms],
      after every executed plan (the strategy waits for that snapshot before placing
      again), when an order update reports an order leaving the book, and after a
      reconnect.
    - [Strategy.tick] runs after every book update, fill and poll. A non-empty plan is
      sent as at most two venue calls: one [cancel] with every cancel, then one [place]
      with every place.

    Ticks, plan execution and polls run one at a time on a sequencer, so a snapshot always
    reflects the plans sent before it. Market data and fills are applied as they arrive. *)

open! Core
open! Async
open Exchange_core

module Report : sig
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

  val report : t -> Report.t
  val strategy : t -> Strategy.t

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
      [params], then {!run}. Closes the venue on every path. *)
  val connect_and_run
    :  ?time_source:Time_source.t
    -> V.config
    -> symbol:Symbol.t
    -> params:(Instrument.t -> mid:Price.t -> Params.t Or_error.t)
    -> summary_interval:Time_ns.Span.t
    -> stop:unit Deferred.t
    -> Report.t Or_error.t Deferred.t
end
