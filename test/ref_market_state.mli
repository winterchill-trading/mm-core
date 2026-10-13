(* Reference copy of the pure lib/market_state.mli from before the allocation work; the
   bisimulation test checks the library against it. *)

(** What the strategy knows about the market: the last book snapshot, mid, microprice and
    an EWMA volatility estimate of mid log returns.

    - mid = (best bid + best ask) / 2
    - microprice = (bid * ask_size + ask * bid_size) / (bid_size + ask_size), which leans
      towards the side with less resting size (the side more likely to be taken out next)
    - with r = ln(mid_k / mid_(k-1)) and alpha = 2 / (vol_window + 1): var_k = (1 -
      alpha) * var_(k-1) + alpha * r^2 (the first return seeds var = r^2), vol_bps =
      10_000 * sqrt(var), a per-update volatility. *)

open! Core
open Exchange_core

type t [@@deriving sexp_of]

val create : vol_window:int -> t

(** Changes the EWMA span; the estimate so far is kept and smoothed with the new span from
    the next update on. *)
val with_vol_window : t -> vol_window:int -> t

(** Applies a book snapshot. One-sided or empty books clear the mid and microprice but do
    not update the volatility estimate. The book's [time] becomes [updated_at]. Books for
    other symbols are not filtered here. *)
val on_book : t -> L2.t -> t

val book : t -> L2.t option
val mid : t -> Price.t option
val microprice : t -> Price.t option
val best_bid : t -> Price.t option
val best_ask : t -> Price.t option

(** Per-update volatility of mid log returns, in bps. [0.] before two two-sided books. *)
val vol_bps : t -> float

(** Number of returns folded into the volatility estimate. *)
val vol_samples : t -> int

(** Overrides the volatility estimate, e.g. to seed it from history before the first
    updates arrive. *)
val with_vol_bps : t -> float -> t

(** Time of the last book, in nanoseconds since the epoch. *)
val updated_at : t -> int option

(** [now - updated_at] in milliseconds (rounded down), [now] in nanoseconds since the
    epoch. [None] before the first book. *)
val age_ms : t -> now:int -> int option
