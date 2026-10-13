(** Small arithmetic helpers over the fixed-point types of [exchange-core].

    Products and quotients of distinct types go through [Fixed_decimal.Product], so they
    are exact up to the last of the 8 decimals and truncate toward zero. Basis-point
    shifts go through [float], which is exact to well below a tick for the prices a market
    maker quotes. *)

open! Core
open Exchange_core

(** [price * size] as a USD amount, truncated toward zero at [10^-8]. *)
val notional : Price.t -> Size.t -> Usd.t

(** [usd / price] as a size, truncated toward zero. Zero for a non-positive price. *)
val size_of_notional : Usd.t -> Price.t -> Size.t

(** [px * (1 + bps / 10_000)], rounded to the nearest [10^-8] (no tick rounding). *)
val shift_bps : Price.t -> float -> Price.t

(** Signed distance from [base] to [px] in basis points of [base]. *)
val bps_between : base:Price.t -> Price.t -> float

(** Largest multiple of [step] that is [<= units]. [step > 0]. *)
val floor_to : int -> step:int -> int

(** Smallest multiple of [step] that is [>= units]. [step > 0]. *)
val ceil_to : int -> step:int -> int

(** Price rounding to a tick. *)
val price_floor : Price.t -> tick:Price.t -> Price.t

val price_ceil : Price.t -> tick:Price.t -> Price.t

(** Size truncated down to a lot multiple (toward zero for positive sizes). *)
val size_floor : Size.t -> lot:Size.t -> Size.t

(** [-1], [0] or [1]. *)
val sign_of_size : Size.t -> int

(** The signed change in position a fill or order on [side] of [size] causes. *)
val signed : Side.t -> Size.t -> Size.t

(** Unboxed versions of the float conversions above, for hot paths: the same results as
    [Price.to_float], [Size.to_float], [Price.of_float] and {!shift_bps}, without boxing
    the float across the call. *)

val price_to_float_u : Price.t -> Stdlib_upstream_compatible.Float_u.t
val size_to_float_u : Size.t -> Stdlib_upstream_compatible.Float_u.t

(** Raises like [Price.of_float] on NaN, infinity or out of range input. *)
val price_of_float_u : Stdlib_upstream_compatible.Float_u.t -> Price.t

val shift_bps_u : Price.t -> Stdlib_upstream_compatible.Float_u.t -> Price.t
val bps_between_u : base:Price.t -> Price.t -> Stdlib_upstream_compatible.Float_u.t
