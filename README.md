# mm-core

Venue-neutral market-making core

Built with [OxCaml](https://oxcaml.org) and the Jane Street library stack (Base, Core,
Async), on the primitives of
[exchange-core](https://github.com/winterchill-trading/exchange-core) (`Price`, `Size`,
`Usd`, `Side`, `Symbol`, `L2`, `Open_order`, `Order_request`, `Order_status`,
`Account_view`, `Instrument`, ...).

The strategy is pure: no networking, no clocks, no randomness. Every function takes the
current time and exchange data as arguments, so the same code runs under tests and under
the live event loop. The live loop (`Live.Make`) is a functor over `Venue.S`, an Async
client signature; a venue adapter implements it over its own wire protocol and the same
strategy runs on Hyperliquid, Binance, Bybit or a simulator.
[hl-mm](https://github.com/winterchill-trading/hl-mm) is the Hyperliquid adapter and
executable.

## Build

```sh
opam switch create 5.2.0+ox --repos ox=git+https://github.com/oxcaml/opam-repository.git,default
opam pin add -n fixed-decimal https://github.com/winterchill-trading/fixed-decimal.git
opam pin add -n exchange-core https://github.com/winterchill-trading/exchange-core.git
opam install . --deps-only --with-test
dune build && dune test
```

Inside a dune workspace that also contains `fixed-decimal` and `exchange-core`, dune uses
the sibling sources and the pins are not needed.

## Modules

All modules live under `Mm_core`.

| Module | Description |
| --- | --- |
| `Fx` | Fixed-point helpers: notionals via `Fixed_decimal.Product`, bps shifts, tick and lot rounding |
| `Params` | Strategy parameters, BTC defaults (`Params.btc`), `for_instrument` scaling, `validate` |
| `Fill` | One of the account's fills: trade id, order id, symbol, side, price, size, fee, maker flag, time, optional closed PnL and client id |
| `Order_update` | Order lifecycle event: `New`, `Partially_filled`, `Filled`, `Cancelled`, `Rejected`, filled and remaining size |
| `Market_state` | Last book, mid, microprice and EWMA volatility of mid returns |
| `Inventory` | Position, average entry, realized/unrealized PnL and fees from fills, synced from account snapshots |
| `Quote` | One desired order: side, level, price, size |
| `Quoter` | Pure `quote : Params.t -> Market_state.t -> Inventory.t -> Quote.t list` |
| `Action` | `Keep oid \| Cancel oid \| Place Order_request.t` |
| `Reconciler` | Minimal plan from resting orders to the desired ladder; deterministic client order ids |
| `Risk` | Kill switch (`Ok \| Halt reason`) and the flatten plan |
| `Strategy` | State machine: `on_book`, `on_fill`, `on_open_orders`, `on_account`, `tick ~now` |
| `Venue` | `module type S`: the Async venue client the live loop runs on |
| `Live` | `Make (V : Venue.S)`: feeds, polls, plan execution, shutdown; the run `Report`; `stop_signal` (SIGINT/SIGTERM) |

Times are nanoseconds since the epoch everywhere (as in `exchange-core`); durations in
`Params` are milliseconds.

## The venue signature

```ocaml
module type S = sig
  type t
  type config

  val connect : config -> t Or_error.t Deferred.t
  val instrument : t -> Symbol.t -> Instrument.t Or_error.t Deferred.t
  val books : t -> Symbol.t -> L2.t Pipe.Reader.t Deferred.t
  val fills : t -> Fill.t Pipe.Reader.t
  val order_updates : t -> Order_update.t Pipe.Reader.t
  val open_orders : t -> Symbol.t -> Open_order.t list Or_error.t Deferred.t
  val account : t -> Account_view.t Or_error.t Deferred.t
  val place : t -> Order_request.t list -> Order_status.t list Or_error.t Deferred.t
  val cancel : t -> Symbol.t -> Order_id.t list -> unit Or_error.t Deferred.t
  val cancel_all : t -> Symbol.t -> unit Or_error.t Deferred.t
  val on_reconnect : t -> f:(unit -> unit) -> unit
  val close : t -> unit Deferred.t
end
```

Contract (see `venue.mli`):

- `config` is venue-specific: endpoints, account, credentials.
- `connect` starts the account feeds; `fills` and `order_updates` are single readers that
  buffer from `connect` on. `books` returns a fresh reader per call.
- **Fills are de-duplicated by trade id**: each trade id at most once per connection,
  including across reconnects and the snapshots some venues resend on resubscription, and
  never fills from before `connect`.
- Implementations reconnect and resubscribe by themselves, then call the `on_reconnect`
  hooks so the loop can resync open orders and the account.
- `place` returns one `Order_status.t` per request, in order; per-order rejections are
  `Rejected reason`, not `Error`. Cancelling an order that is already gone is not an
  error.
- `instrument` returns the tick valid at the current price for venues whose tick depends
  on it (Hyperliquid's 5 significant figures).

## Live loop

```ocaml
module Live_hl = Mm_core.Live.Make (Hl_venue)

Live_hl.connect_and_run config ~symbol
  ~params:(fun instrument ~mid -> Params.validate (Params.for_instrument instrument ~mid))
  ~summary_interval:(Time_ns.Span.of_int_sec 10)
  ~stop:(Live.stop_signal ?run_for ())
```

- `books` and `fills` feed `Strategy.on_book` and `Strategy.on_fill`; the loop also
  de-duplicates fills by trade id.
- `open_orders` and `account` are polled every `requote_interval_ms`, after every
  executed plan (the strategy waits for that snapshot before placing again), when an order
  update reports an order leaving the book, and after a reconnect.
- `Strategy.tick` runs after every book update, fill and poll. A plan becomes at most two
  calls: one `cancel` with all cancels, then one `place` with all places.
- Ticks, plan execution and polls run one at a time on an Async sequencer, so a snapshot
  never predates a plan the strategy already sent. Requests are coalesced: at most one
  tick and one poll wait in the queue.
- `stop` cancels every resting order on the symbol (`cancel_all`, re-checking open orders
  up to three times), takes a final account snapshot, closes the venue and returns the
  report. `stop_signal` resolves on SIGINT/SIGTERM or after `run_for`; a second signal
  exits with code 130 at once. The position is not flattened on exit.
- The report's cancellation count comes from `Cancelled` order updates.

## Strategy

The quoter is a simplified Avellaneda-Stoikov market maker. All spreads are in basis
points (bps) of the price.

### Market inputs

```
mid         = (best_bid + best_ask) / 2
microprice  = (best_bid * ask_size + best_ask * bid_size) / (bid_size + ask_size)
r_k         = ln(mid_k / mid_(k-1))                      on each two-sided book
var_k       = (1 - a) * var_(k-1) + a * r_k^2,  a = 2 / (vol_window + 1)
sigma       = min(10_000 * sqrt(var), max_vol_bps)       per-update volatility, bps
```

### Ladder

```
cap   = min(max_position, max_notional / mid)            position capacity, base units
q     = clamp((position - inventory_target) / cap, -1, 1)
v     = gamma * sigma^2                                  risk term, bps
r     = microprice * (1 - q * (skew_bps + v) / 10_000)   reservation price
delta = base_half_spread_bps + v / 2                     half-spread, bps
d_i   = delta + i * level_spacing_bps                    level i = 0 .. levels-1

bid_i = floor_tick(r * (1 - d_i / 10_000))   capped at best_ask - tick and bid_(i-1) - tick
ask_i = ceil_tick (r * (1 + d_i / 10_000))   floored at best_bid + tick and ask_(i-1) + tick
```

When long (`q > 0`) both sides move down: the bid backs away and the ask gets more
aggressive, and vice versa when short. Higher volatility widens the spread and
strengthens the skew.

Sizes are `order_size` per level, truncated to the lot, handed out from level 0 until the
room on that side runs out (`cap - position` for bids, `cap + position` for asks), so a
side at the limit gets no quotes and filling every quote never takes the position beyond
`cap <= max_position`. A property test checks that quotes never cross (each other or the
book) and never breach the limit.

### Reconciliation

Each desired quote keeps the closest resting order on the same side with exactly its size,
within `requote_threshold_bps` of its price and younger than `order_ttl_ms`. Unmatched
resting orders are cancelled; unmatched quotes are placed as post-only (`Alo`) limit
orders. Client order ids are `0x` plus 32 hex digits encoding
`symbol (8 bytes) | side (1) | level (1) | generation (6)`: reproducible, readable, and a
valid client id on Hyperliquid (128-bit cloid), Binance and Bybit (at most 36 characters).

### Risk

| Check | Halt | Response |
| --- | --- | --- |
| `abs(position) > max_position` | sticky | cancel all, reduce-only IOC for the whole position at `mid -/+ flatten_slippage_bps` |
| `peak_pnl - pnl > max_drawdown`, `pnl = realized + unrealized(mid) - fees` | sticky | same |
| no two-sided book | until data | cancel all |
| `now - book_time > max_data_age_ms` | until data | cancel all |

### Strategy loop

`tick ~now` folds the above together. It requotes when the last plan is older than
`requote_interval_ms`, the mid moved by `requote_threshold_bps`, or the position changed.
After sending a plan it waits for an open-orders snapshot (up to `requote_interval_ms`)
before planning again, and without one it only cancels, so orders are never placed twice.

### Inventory accounting

Average cost: fills that grow the position move the entry to the size-weighted mean;
fills that reduce it realize `closed * (px - entry) * sign(position)`; a fill that flips
the position reopens the remainder at the fill price. An account snapshot overwrites
position and entry; fills at or before the snapshot time then add only their fee and the
venue's `closed_pnl` (nothing if the venue does not report one). Fills are de-duplicated
by trade id.

### Defaults (`Params.btc`)

| Parameter | Value |
| --- | --- |
| tick / lot | 1 / 0.00001 |
| order size x levels | 0.001 x 3 |
| base half-spread / level spacing | 3 bps / 2 bps |
| skew / gamma / max vol | 4 bps / 0.5 / 10 bps |
| max position / max notional | 0.01 BTC / 1500 USD |
| requote threshold / interval / order ttl | 1 bps / 1 s / 60 s |
| max drawdown / max data age / flatten slippage | 50 USD / 5 s / 50 bps |

`Params.for_instrument instrument ~mid` rescales these to another instrument: about 100
USD per order (nearest lot), 10 orders of position room and 1.5x that in notional.

## Tests

`dune test` runs the strategy's expect and `Base_quickcheck` tests (quotes never cross or
breach the limit; reconciling a ladder against itself is a no-op) and a live-loop test
over an in-memory `Venue.S` implementation: it quotes the expected ladder, applies a fill
delivered twice only once, and on stop cancels every order and closes the venue.
