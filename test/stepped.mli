(** A deterministic stepped harness: drives {!Mm_core.Strategy} from a scripted list of
    book updates through a toy exchange, in lockstep, with fills drawn from a seeded
    random state. Same script and seed, same output.

    Each step, in order:
    + the scripted book is applied and [now] moves to its time;
    + the exchange fills resting orders: a post-only order the new book trades through
      fills in full at its price; any other fills with probability [fill_probability],
      halved per whole basis point between its price and the touch on its side;
    + the fills go to [Strategy.on_fill] and, if there were any, the resting orders to
      [Strategy.on_open_orders] (as the live loop polls when an order leaves the book),
      then [Strategy.tick];
    + the plan is executed: cancels, then places (post-only orders rest unless they would
      cross; IOC orders fill against the book's touch, or are dropped);
    + [Strategy.on_open_orders] gets the resting orders. *)

open! Core
open Exchange_core
open Mm_core

module Step : sig
  type t =
    { time_ms : int
    ; book : L2.t
    ; fills : Fill.t list
    ; plan : Action.t list
    ; strategy : Strategy.t
    }
end

val run
  :  ?fill_probability:float (** default 0.3 *)
  -> seed:int
  -> Params.t
  -> (int * string * string) list (** time in ms, best bid, best ask *)
  -> Step.t list

(** One line per step: time, book, fills, plan counts, position, PnL at the mid, mode. *)
val print : Step.t list -> unit
