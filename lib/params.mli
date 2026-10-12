(** Strategy parameters. Prices, sizes and amounts use the fixed-point types of
    [exchange-core]; spreads and thresholds are in basis points of the price. Durations
    are milliseconds (timestamps elsewhere in this library are nanoseconds since the
    epoch, as in [exchange-core]). *)

open! Core
open Exchange_core

type t =
  { symbol : Symbol.t (** instrument to quote *)
  ; tick : Price.t (** price increment; quotes are rounded to multiples of it *)
  ; lot : Size.t (** size increment; quote sizes are truncated to multiples of it *)
  ; order_size : Size.t (** size quoted at each level *)
  ; levels : int (** number of levels per side *)
  ; base_half_spread_bps : float
  (** distance of the first level from the reservation price *)
  ; level_spacing_bps : float (** extra distance per additional level *)
  ; skew_bps : float
  (** reservation price shift per unit of inventory ratio (position / max position) *)
  ; max_position : Size.t (** absolute position limit, in base units *)
  ; max_notional : Usd.t (** absolute position limit, in USD at the mid *)
  ; inventory_target : Size.t (** position the quoter skews towards *)
  ; requote_threshold_bps : float
  (** a resting order within this distance of its desired price (and with the desired
      size) is kept; the mid must move this far to trigger an early requote *)
  ; requote_interval_ms : int
  (** minimum time between plans, unless the mid moved past the threshold or the position
      changed; also the timeout waiting for an open orders snapshot *)
  ; order_ttl_ms : int (** resting orders older than this are replaced *)
  ; gamma : float (** risk aversion, per bps of variance *)
  ; vol_window : int (** EWMA span (in book updates) of the volatility estimate *)
  ; max_vol_bps : float
  (** cap on the volatility fed to the quoter, so a jump in the book cannot blow up the
      [gamma * sigma^2] term *)
  ; max_drawdown : Usd.t (** kill switch: peak-to-current PnL drop *)
  ; max_data_age_ms : int (** kill switch: book age before quotes are pulled *)
  ; flatten_slippage_bps : float (** limit price offset of the reduce-only IOC flatten *)
  ; max_actions_per_sec : int
  (** kill switch: more cancels and places than this in the last second (absent in a
      sexp: 100) *)
  }
[@@deriving sexp, compare, equal]

(** Defaults for BTC perpetuals around 100k (integer tick, 5 size decimals): 3 levels of
    0.001 BTC, 3 bps half-spread, 2 bps spacing, 0.01 BTC / 1500 USD position limit,
    volatility capped at 10 bps, 50 USD drawdown limit, at most 100 actions per second. *)
val btc : t

(** [btc] rescaled to an instrument trading around [mid]: symbol, [tick] (default the
    instrument's [price_tick]) and lot from the instrument, [order_size] about 100 USD at
    [mid] rounded to the nearest lot (at least one), [max_position] 10 orders and
    [max_notional] 1.5 x [max_position] at [mid]. For BTC at 100k this reproduces [btc].

    With the venue's [min_notional] (order value minimum, e.g. 100 USD on Binance's
    BTCUSDT), the default [order_size] is at least [1.2 * min_notional] at [mid], rounded
    up to the lot, so quotes a few bps below the mid still clear the minimum. *)
val for_instrument
  :  ?tick:Price.t
  -> ?order_size:Size.t
  -> ?max_position:Size.t
  -> ?min_notional:Usd.t
  -> Instrument.t
  -> mid:Price.t
  -> t

(** Checks the invariants the quoter relies on (positive tick, lot and size, positive
    half-spread, at least one level, ...). *)
val validate : t -> t Or_error.t
