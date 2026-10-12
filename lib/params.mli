(** Strategy parameters. Prices, sizes and amounts use the fixed-point types of
    [exchange-core]; spreads and thresholds are in basis points of the price. Durations
    are [Time_ns.Span.t]; the [_ms] suffix of their names is kept so params files stay
    readable. In a sexp a duration prints as a span ([1s], [500ms]) and reads from either
    a span or a bare integer of milliseconds (the format of older params files), so
    [(requote_interval_ms 1000)] and [(requote_interval_ms 1s)] are the same. Timestamps
    elsewhere in this library are nanoseconds since the epoch, as in [exchange-core]. *)

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
  ; requote_interval_ms : Time_ns.Span.t
  (** minimum time between plans, unless the mid moved past the threshold or the position
      changed; also the timeout waiting for an open orders snapshot *)
  ; order_ttl_ms : Time_ns.Span.t (** resting orders older than this are replaced *)
  ; gamma : float (** risk aversion, per bps of variance *)
  ; vol_window : int (** EWMA span (in book updates) of the volatility estimate *)
  ; max_vol_bps : float
  (** cap on the volatility fed to the quoter, so a jump in the book cannot blow up the
      [gamma * sigma^2] term *)
  ; max_drawdown : Usd.t (** kill switch: peak-to-current PnL drop *)
  ; max_data_age_ms : Time_ns.Span.t (** kill switch: book age before quotes are pulled *)
  ; flatten_slippage_bps : float (** limit price offset of the reduce-only IOC flatten *)
  ; max_actions_per_sec : int
  (** kill switch: more cancels and places than this in the last second (absent in a
      sexp: 100) *)
  ; soft_max_position : Size.t option
  (** beyond this absolute position the side that grows it is pulled, instead of halting
      as at [max_position] (absent in a sexp: none) *)
  ; bump_budget : Size.t
  (** how much, in total over a run, fills may take the position past [soft_max_position]
      while the growing side keeps quoting (absent in a sexp: 0) *)
  ; action_rate_grace_ms : Time_ns.Span.t
  (** after a venue resync ({!Strategy.on_resync}) the actions of this long are neither
      counted nor checked against [max_actions_per_sec], so the requote burst of a
      reconnect cannot trip the kill switch (absent in a sexp: 5000, and omitted when
      printing at that value) *)
  ; action_rate_cooldown_ms : Time_ns.Span.t option
  (** when set, a [Max_action_rate] halt clears this long after it tripped and the
      strategy quotes again; other halts stay permanent (absent in a sexp: none, the halt
      is permanent) *)
  }
[@@deriving sexp, compare, equal]

(** Defaults for BTC perpetuals around 100k (integer tick, 5 size decimals): 3 levels of
    0.001 BTC, 3 bps half-spread, 2 bps spacing, 0.01 BTC / 1500 USD position limit,
    volatility capped at 10 bps, 50 USD drawdown limit, at most 100 actions per second
    outside a 5 s grace period after a resync, no action rate cooldown. *)
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
    half-spread, at least one level, ...). Every float field must be finite: NaN and
    infinities are rejected, whatever the field's range. *)
val validate : t -> t Or_error.t
