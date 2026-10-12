(** Drives {!Mm_core.Strategy} from recorded market data: every L2 update of a
    {!Market_data.Feed.t} (for example a [Market_data.Replay_feed]) is applied to a book,
    and once the book has both sides the strategy sees it and ticks at the update's
    [recv_ts].

    There is no exchange: a plan is assumed to be accepted at once (places rest with
    sequential order ids from 1, cancels remove), and the resulting open orders are fed
    back through [Strategy.on_open_orders] before the next update. Nothing fills, so the
    output shows quoting and requoting only. *)

open! Core
open! Async
open Exchange_core
open Mm_core

module Tick : sig
  type t =
    { time : Time_ns.t (** [recv_ts] of the update *)
    ; bid : Price.t option
    ; ask : Price.t option
    ; mode : Strategy.Mode.t
    ; plan : Action.t list (** empty when nothing was sent *)
    }
  [@@deriving sexp_of]

  (** A header line (UTC time, best bid and ask, mode), then one indented line per cancel
      and place of the plan, and a count of the orders kept. *)
  val to_string_hum : t -> string
end

(** Reads [feed]'s L2 updates until it ends, calling [f] for every tick. [params] builds
    the parameters from the mid of the first two-sided book; the strategy is created
    there. [depth] (default 20) limits each side of the book the strategy sees. Closes the
    feed's other pipes at once and the feed itself at the end. Returns the final strategy,
    or [None] when no update made a two-sided book. *)
val run
  :  ?depth:int
  -> Market_data.Feed.t
  -> params:(mid:Price.t -> Params.t Or_error.t)
  -> f:(Tick.t -> unit)
  -> Strategy.t option Or_error.t Deferred.t

(** Default parameters from the built-in instrument catalogue
    ([Market_data.Instrument_info]): its tick and lot, then {!Params.for_instrument} at
    [mid]. Fails for an instrument missing from the catalogue or whose lot is not a power
    of ten. *)
val catalogue_params : Market_data.Instrument.t -> mid:Price.t -> Params.t Or_error.t

(** The instrument under the store [dir] named [symbol], either [venue:SYMBOL] or a bare
    symbol found on exactly one venue, and its norm/ L2 files for every recorded day. *)
val l2_files
  :  dir:string
  -> symbol:string
  -> (Market_data.Instrument.t * string list) Or_error.t Deferred.t
