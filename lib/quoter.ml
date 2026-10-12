open! Core
open Exchange_core

module Model = struct
  type t =
    { microprice : Price.t
    ; mid : Price.t
    ; vol_bps : float
    ; capacity : Size.t
    ; inventory_ratio : float
    ; reservation : Price.t
    ; half_spread_bps : float
    }
  [@@deriving sexp_of]
end

let capacity (p : Params.t) ~mid =
  Fx.size_floor
    (Size.min p.max_position (Fx.size_of_notional p.max_notional mid))
    ~lot:p.lot
;;

let model (p : Params.t) market inventory =
  match Market_state.microprice market, Market_state.mid market with
  | Some microprice, Some mid ->
    let vol_bps = Float.min (Market_state.vol_bps market) p.max_vol_bps in
    let capacity = capacity p ~mid in
    let excess = Size.(Inventory.position inventory - p.inventory_target) in
    let inventory_ratio =
      if Size.is_positive capacity
      then
        Float.clamp_exn
          (Size.to_float excess /. Size.to_float capacity)
          ~min:(-1.)
          ~max:1.
      else 0.
    in
    let v = p.gamma *. vol_bps *. vol_bps in
    let reservation = Fx.shift_bps microprice (-.inventory_ratio *. (p.skew_bps +. v)) in
    let half_spread_bps = p.base_half_spread_bps +. (v /. 2.) in
    Some
      { Model.microprice
      ; mid
      ; vol_bps
      ; capacity
      ; inventory_ratio
      ; reservation
      ; half_spread_bps
      }
  | _ -> None
;;

(* Sizes for each level given the room left on a side. *)
let level_sizes (p : Params.t) ~room =
  let rec go level room acc =
    if level >= p.levels
    then List.rev acc
    else (
      let sz = Fx.size_floor (Size.min p.order_size room) ~lot:p.lot in
      if Size.is_positive sz
      then go (level + 1) Size.(room - sz) (sz :: acc)
      else List.rev acc)
  in
  if Size.is_positive room then go 0 room [] else []
;;

let quote (p : Params.t) market inventory =
  match model p market inventory with
  | None -> []
  | Some m ->
    let x = Inventory.position inventory in
    let distance level =
      m.half_spread_bps +. (Float.of_int level *. p.level_spacing_bps)
    in
    let best_bid = Market_state.best_bid market in
    let best_ask = Market_state.best_ask market in
    let bids =
      level_sizes p ~room:Size.(m.capacity - x)
      |> List.folding_mapi ~init:None ~f:(fun level prev sz ->
        let px =
          Fx.price_floor (Fx.shift_bps m.reservation (-.distance level)) ~tick:p.tick
        in
        let px =
          Option.fold best_ask ~init:px ~f:(fun px ask ->
            Price.min px Price.(ask - p.tick))
        in
        let px =
          Option.fold prev ~init:px ~f:(fun px prev -> Price.min px Price.(prev - p.tick))
        in
        Some px, { Quote.side = Buy; level; px; sz })
      |> List.filter ~f:(fun (q : Quote.t) -> Price.is_positive q.px)
    in
    let asks =
      level_sizes p ~room:Size.(m.capacity + x)
      |> List.folding_mapi ~init:None ~f:(fun level prev sz ->
        let px =
          Fx.price_ceil (Fx.shift_bps m.reservation (distance level)) ~tick:p.tick
        in
        let px =
          Option.fold best_bid ~init:px ~f:(fun px bid ->
            Price.max px Price.(bid + p.tick))
        in
        let px =
          Option.fold prev ~init:px ~f:(fun px prev -> Price.max px Price.(prev + p.tick))
        in
        Some px, { Quote.side = Sell; level; px; sz })
    in
    bids @ asks
;;
