(** The desired quote ladder, as a pure function of parameters, market and inventory.

    With microprice [m], per-update volatility [sigma = min(vol_bps, max_vol_bps)] (bps),
    position [x]:

    - capacity [cap = min(max_position, max_notional / mid)]
    - inventory ratio [q = clamp((x - inventory_target) / cap, -1, 1)]
    - risk term [v = gamma * sigma^2] (bps)
    - reservation price [r = m * (1 - q * (skew_bps + v) / 10_000)]
    - half-spread [delta = base_half_spread_bps + v / 2] (bps)
    - level [i] (from 0) at distance [d_i = delta + i * level_spacing_bps]: bid
      [floor_tick(r * (1 - d_i / 10_000))], ask [ceil_tick(r * (1 + d_i / 10_000))]
    - bids are capped at [best_ask - tick] and asks floored at [best_bid + tick] so they
      stay post-only; each level is at least one tick beyond the previous one
    - sizes: [order_size] per level, truncated to the lot, filled greedily from level 0
      until the room on that side is used up: [cap - x] for bids, [cap + x] for asks. A
      side with no room gets no quotes, so fully filling every quote never takes
      [|position|] past [cap <= max_position]. *)

open! Core
open Exchange_core

module Model : sig
  type t =
    { microprice : Price.t
    ; mid : Price.t
    ; vol_bps : float (** after the [max_vol_bps] cap *)
    ; capacity : Size.t
    ; inventory_ratio : float
    ; reservation : Price.t
    ; half_spread_bps : float
    }
  [@@deriving sexp_of]
end

(** The intermediate values of the formulas above, [None] without a two-sided book. *)
val model : Params.t -> Market_state.t -> Inventory.t -> Model.t option

(** Bids (level order) followed by asks (level order). Empty without a two-sided book. *)
val quote : Params.t -> Market_state.t -> Inventory.t -> Quote.t list

(** Why a side has no quotes. *)
module Unquoted : sig
  type t =
    | No_book (** no two-sided book *)
    | At_limit of
        { position : Size.t
        ; capacity : Size.t
        } (** the side would take [|position|] past [capacity] *)
    | Room_below_lot of { room : Size.t } (** the room left is less than one lot *)
    | Non_positive_price
    (** every rung priced at or below zero (a bid far from the mid) *)
  [@@deriving sexp, compare, equal]
end

(** [quote], and a reason for each side left without quotes. *)
val quote_with_reasons
  :  Params.t
  -> Market_state.t
  -> Inventory.t
  -> Quote.t list * (Side.t * Unquoted.t) list

(** Only the reasons of [quote_with_reasons]. *)
val unquoted : Params.t -> Market_state.t -> Inventory.t -> (Side.t * Unquoted.t) list

module For_testing : sig
  (** Every term of the computation above as an indented tree: book, model terms, and per
      side the room and each rung's distance, unrounded target, tick-rounded and final
      price, then the reason for a side without quotes. *)
  val explain : Params.t -> Market_state.t -> Inventory.t -> string
end
