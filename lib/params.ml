open! Core
open Exchange_core

type t =
  { symbol : Symbol.t
  ; tick : Price.t
  ; lot : Size.t
  ; order_size : Size.t
  ; levels : int
  ; base_half_spread_bps : float
  ; level_spacing_bps : float
  ; skew_bps : float
  ; max_position : Size.t
  ; max_notional : Usd.t
  ; inventory_target : Size.t
  ; requote_threshold_bps : float
  ; requote_interval_ms : int
  ; order_ttl_ms : int
  ; gamma : float
  ; vol_window : int
  ; max_vol_bps : float
  ; max_drawdown : Usd.t
  ; max_data_age_ms : int
  ; flatten_slippage_bps : float
  ; max_actions_per_sec : int [@default 100]
  ; soft_max_position : Size.t option [@default None]
  ; bump_budget : Size.t [@default Size.zero]
  }
[@@deriving sexp, compare, equal]

let btc =
  { symbol = Symbol.of_string "BTC"
  ; tick = Price.of_string "1"
  ; lot = Size.of_string "0.00001"
  ; order_size = Size.of_string "0.001"
  ; levels = 3
  ; base_half_spread_bps = 3.
  ; level_spacing_bps = 2.
  ; skew_bps = 4.
  ; max_position = Size.of_string "0.01"
  ; max_notional = Usd.of_string "1500"
  ; inventory_target = Size.zero
  ; requote_threshold_bps = 1.
  ; requote_interval_ms = 1_000
  ; order_ttl_ms = 60_000
  ; gamma = 0.5
  ; vol_window = 100
  ; max_vol_bps = 10.
  ; max_drawdown = Usd.of_string "50"
  ; max_data_age_ms = 5_000
  ; flatten_slippage_bps = 50.
  ; max_actions_per_sec = 100
  ; soft_max_position = None
  ; bump_budget = Size.zero
  }
;;

let min_notional_safety = 1.2

let default_order_size ?min_notional ~lot ~mid () =
  let lot = Size.to_units lot in
  (* about 100 USD, nearest lot, at least one *)
  let units = Size.to_units (Fx.size_of_notional (Usd.of_int 100) mid) in
  let around_100 = Int.max lot ((units + (lot / 2)) / lot * lot) in
  match min_notional with
  | None -> Size.of_units around_100
  | Some min_notional ->
    (* comfortably above the venue minimum, so quotes below the mid still pass it *)
    let target = Usd.of_float (min_notional_safety *. Usd.to_float min_notional) in
    let units = Size.to_units (Fx.size_of_notional target mid) in
    let units =
      if Usd.(Fx.notional mid (Size.of_units units) < target) then units + 1 else units
    in
    let floor = Fx.ceil_to (Int.max 1 units) ~step:lot in
    Size.of_units (Int.max around_100 floor)
;;

let for_instrument
  ?tick
  ?order_size
  ?max_position
  ?min_notional
  (instrument : Instrument.t)
  ~mid
  =
  let lot = Instrument.lot_size instrument in
  let order_size =
    match order_size with
    | Some order_size -> order_size
    | None -> default_order_size ?min_notional ~lot ~mid ()
  in
  let max_position = Option.value max_position ~default:(Size.scale order_size ~by:10) in
  { btc with
    symbol = instrument.symbol
  ; tick = Option.value tick ~default:instrument.price_tick
  ; lot
  ; order_size
  ; max_position
  ; max_notional = Usd.of_float (1.5 *. Size.to_float max_position *. Price.to_float mid)
  }
;;

let validate t =
  (* Every field is named, so a new one cannot be added without deciding its checks. *)
  let { symbol = _
      ; tick
      ; lot
      ; order_size
      ; levels
      ; base_half_spread_bps
      ; level_spacing_bps
      ; skew_bps
      ; max_position
      ; max_notional
      ; inventory_target
      ; requote_threshold_bps
      ; requote_interval_ms
      ; order_ttl_ms
      ; gamma
      ; vol_window
      ; max_vol_bps
      ; max_drawdown
      ; max_data_age_ms
      ; flatten_slippage_bps
      ; max_actions_per_sec
      ; soft_max_position
      ; bump_budget
      }
    =
    t
  in
  let finite =
    List.map
      [ "base_half_spread_bps", base_half_spread_bps
      ; "level_spacing_bps", level_spacing_bps
      ; "skew_bps", skew_bps
      ; "requote_threshold_bps", requote_threshold_bps
      ; "gamma", gamma
      ; "max_vol_bps", max_vol_bps
      ; "flatten_slippage_bps", flatten_slippage_bps
      ]
      ~f:(fun (name, x) -> Float.is_finite x, name ^ " must be finite")
  in
  let checks =
    finite
    @ [ Price.is_positive tick, "tick must be positive"
      ; Size.is_positive lot, "lot must be positive"
      ; Size.(order_size >= lot), "order_size must be at least one lot"
      ; levels >= 1 && levels <= 255, "levels must be in [1, 255]"
      ; Float.(base_half_spread_bps > 0.), "base_half_spread_bps must be positive"
      ; Float.(level_spacing_bps >= 0.), "level_spacing_bps must be non-negative"
      ; Float.(skew_bps >= 0.), "skew_bps must be non-negative"
      ; Size.is_positive max_position, "max_position must be positive"
      ; Usd.is_positive max_notional, "max_notional must be positive"
      ; ( Size.(abs inventory_target < max_position)
        , "inventory_target must be inside max_position" )
      ; Float.(requote_threshold_bps >= 0.), "requote_threshold_bps must be non-negative"
      ; requote_interval_ms > 0, "requote_interval_ms must be positive"
      ; order_ttl_ms > 0, "order_ttl_ms must be positive"
      ; Float.(gamma >= 0.), "gamma must be non-negative"
      ; vol_window >= 1, "vol_window must be positive"
      ; Float.(max_vol_bps >= 0.), "max_vol_bps must be non-negative"
      ; Usd.is_positive max_drawdown, "max_drawdown must be positive"
      ; max_data_age_ms > 0, "max_data_age_ms must be positive"
      ; Float.(flatten_slippage_bps > 0.), "flatten_slippage_bps must be positive"
      ; max_actions_per_sec > 0, "max_actions_per_sec must be positive"
      ; ( Option.for_all soft_max_position ~f:(fun soft ->
            Size.is_positive soft && Size.(soft <= max_position))
        , "soft_max_position must be in (0, max_position]" )
      ; not (Size.is_negative bump_budget), "bump_budget must be non-negative"
      ]
  in
  match List.filter_map checks ~f:(fun (ok, msg) -> Option.some_if (not ok) msg) with
  | [] -> Ok t
  | errors -> Or_error.error_s [%message "invalid params" (errors : string list)]
;;
