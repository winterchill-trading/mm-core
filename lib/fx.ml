open! Core
open Exchange_core
module Notional = Fixed_decimal.Product (Price) (Size) (Usd)
module Quantity = Fixed_decimal.Product (Size) (Price) (Usd)

let notional px sz = Notional.mul px sz

let size_of_notional usd px =
  if Price.is_positive px then Quantity.div usd px else Size.zero
;;

let shift_bps px bps =
  (Price.of_float [@alert "-lossy"]) (Price.to_float px *. (1. +. (bps /. 10_000.)))
;;

let bps_between ~base px =
  let b = Price.to_float base in
  if Float.( = ) b 0. then 0. else (Price.to_float px -. b) /. b *. 10_000.
;;

let floor_to units ~step =
  if units >= 0 then units / step * step else -((-units + step - 1) / step * step)
;;

let ceil_to units ~step = -(floor_to (-units) ~step)

let price_floor px ~tick =
  Price.of_units (floor_to (Price.to_units px) ~step:(Price.to_units tick))
;;

let price_ceil px ~tick =
  Price.of_units (ceil_to (Price.to_units px) ~step:(Price.to_units tick))
;;

let size_floor sz ~lot =
  let u = Size.to_units sz in
  let step = Size.to_units lot in
  Size.of_units (if u >= 0 then floor_to u ~step else -(floor_to (-u) ~step))
;;

let sign_of_size sz = Int.sign (Size.to_units sz) |> Sign.to_int

let signed (side : Side.t) sz =
  match side with
  | Buy -> sz
  | Sell -> Size.neg sz
;;

module Float_u = Stdlib_upstream_compatible.Float_u

(* The same arithmetic as [Price.to_float] and [Price.of_float], done here so that the
   result or argument stays unboxed across the module boundary. *)
let price_scale = Float.of_int (Price.to_units (Price.of_int 1))
let size_scale = Float.of_int (Size.to_units (Size.of_int 1))

let price_to_float_u px =
  Float_u.of_float (Float.of_int (Price.to_units px) /. price_scale)
;;

let size_to_float_u sz = Float_u.of_float (Float.of_int (Size.to_units sz) /. size_scale)

let price_of_float_u f =
  Price.of_units (Float.iround_nearest_exn (Float_u.to_float f *. price_scale))
;;

let shift_bps_u px bps =
  let bps = Float_u.to_float bps in
  price_of_float_u
    (Float_u.of_float
       (Float_u.to_float (price_to_float_u px) *. (1. +. (bps /. 10_000.))))
;;

let bps_between_u ~base px =
  let b = Float_u.to_float (price_to_float_u base) in
  if Float.( = ) b 0.
  then #0.
  else Float_u.of_float ((Float_u.to_float (price_to_float_u px) -. b) /. b *. 10_000.)
;;
