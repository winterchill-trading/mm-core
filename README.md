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
# only for the mm-core-replay package
opam pin add -n parquet https://github.com/winterchill-trading/parquet.git
opam pin add -n market-data https://github.com/winterchill-trading/market-data.git
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
| `Quoter` | Pure `quote : Params.t -> Market_state.t -> Inventory.t -> Quote.t list`; `quote_with_reasons` adds a typed `Unquoted.t` for each side without quotes; `For_testing.explain` prints every term as a tree |
| `Action` | `Keep oid \| Cancel oid \| Place Order_request.t` |
| `Reconciler` | Minimal plan from resting orders to the desired ladder; deterministic client order ids |
| `Risk` | Kill switch (`Ok \| Halt reason`), the one second `Action_rate` window and the flatten plan |
| `Toxicity_gate` | Pure decision from a toxicity signal: quote normally, widen (wider half-spread, reservation shift towards the flow, smaller size) or pull |
| `Reference_guard` | Plausibility check: pull when the venue mid stays outside a band around a reference mid past a grace period, or at once after a reference jump; typed events |
| `Strategy` | State machine: `on_book`, `on_fill`, `on_open_orders`, `on_account`, `on_resync`, `on_reference`, `on_toxicity`, `tick ~now`, `tick_with_events`, `update_params` |
| `Venue` | `module type S`: the Async venue client the live loop runs on |
| `Latency_histogram` | Latency histogram at two significant digits: count, p50, p99, exact max |
| `Live` | `Make (V : Venue.S)`: feeds, polls, plan execution, shutdown; hot `update_params`; the run `Report`; tick-to-order latency; `stop_signal` (SIGINT/SIGTERM) |

Times are nanoseconds since the epoch everywhere (as in `exchange-core`); durations in
`Params` (`requote_interval_ms`, `order_ttl_ms`, `max_data_age_ms`,
`action_rate_grace_ms`, `action_rate_cooldown_ms`) are `Time_ns.Span.t`. Their names keep
the `_ms` suffix so existing params files still read: in a sexp a duration prints as a
span (`1s`, `500ms`) and reads from either a span or a bare integer of milliseconds, so
`(requote_interval_ms 1000)` and `(requote_interval_ms 1s)` are the same.

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
- The report's `gc` field (and a `gc` line in `to_string_hum`) holds the minor and major
  collections since the loop started and the current major heap size in words, from
  `Gc.quick_stat` (no heap walk). OCaml 5's `top_heap_words` is not reported: it reads
  below `heap_words`.
- `tick_to_order` reports p50, p99 and max of the time from a book update's arrival to
  the send of the plan it led to (sequencer waits included).
- `start` and `connect_and_start` return the running loop; `update_params`, `report`,
  `tick_to_order` and `stop` take that handle. `run` and `connect_and_run` wrap them and
  wait for a stop deferred.
- `connect_and_start ?on_venue` (and `connect_and_run`) calls `on_venue` with the venue
  right after connecting, before the first mid is fetched and the loop starts, so
  observers can subscribe to its feeds or register `on_reconnect` hooks without dropping
  down to `start`. An exception it raises is returned as the error and closes the venue.
- `on_resync live ~f` registers a hook run on every venue reconnect, right after the loop
  has called `Strategy.on_resync` on its own strategy. Mirror strategies (a shadow
  strategy, a strategy log replay) call `Strategy.on_resync` on their copy there, so they
  start the same action rate grace period at the same time.
- `start`, `connect_and_start`, `run`, `connect_and_run`, `first_mid` and `stop_signal` take an optional
  `?time_source` (default the wall clock). With `Time_source.create` a whole run is driven
  by `advance_by_alarms`, as the property and latency tests do.

### Hot parameter updates

```ocaml
let%bind live = Live_hl.connect_and_start config ~symbol ~params ~summary_interval in
let live = ok_exn live in
(* later, e.g. on SIGHUP after re-reading a params file *)
Live_hl.update_params live { (Mm_core.Strategy.params (Live_hl.strategy live)) with
  base_half_spread_bps = 5. }
```

`update_params` validates the new parameters (same symbol only) and swaps them into the
running strategy: market state, inventory, resting orders, generation, mode and action
history carry over, the loop keeps running and nothing is cancelled wholesale. The next
tick requotes; the reconciler keeps the resting orders that still match the new ladder
and replaces the rest. A changed `requote_interval_ms` re-arms the poll timer, a changed
`vol_window` keeps the volatility estimate and smooths it with the new span from then
on, and a halted strategy stays halted. `Strategy.update_params` is the pure version.

## Replay

The `mm-core-replay` package (library `Mm_core_replay`, binary `mm-core`) runs the
strategy over recorded market data from
[market-data](https://github.com/winterchill-trading/market-data). Every L2 update of
the instrument, from a `Market_data.Replay_feed` over its norm/ event logs, is applied to
a book; once the book has both sides the strategy sees it and ticks at the update's
`recv_ts`. There is no exchange: plans are accepted as sent (places rest with order ids
from 1, cancels remove) and fed back as the next open orders snapshot, and nothing fills,
so the output shows quoting and requoting only.

```sh
mm-core replay -norm DIR -symbol binance:BTCUSDT [-params FILE] [-depth 20]
```

`DIR` is a store root (`<DIR>/<venue>/<symbol>/norm/l2/<day>.bin`); every recorded day is
replayed. `-symbol` takes `venue:SYMBOL` or a symbol recorded on one venue only. Without
`-params` the parameters are `Params.for_instrument` at the first mid, with the tick and
lot of the built-in `Market_data.Instrument_info` catalogue. Over the fixture in
`replay/test/fixtures` (a copy of market-data's `event-log-v1` Binance log):

```
2026-01-01T00:00:00.001000000Z bid 99.9 ask 100.1 Running
  place buy 1 @ 99.9
  place buy 1 @ 99.8
  place buy 1 @ 99.7
  place sell 1 @ 100
  place sell 1 @ 100.1
  place sell 1 @ 100.2
2026-01-01T00:00:00.022000000Z bid 99.8 ask 100.1 Running
  cancel 4
  place sell 1 @ 100.3
  keep 5
```

`Replay.run feed ~params ~f` is the library form: any `Market_data.Feed.t`, a callback
per `Replay.Tick.t`, the final strategy returned.

## Timeouts and abandoned work

`with_timeout` only stops waiting; the work it raced keeps running. Every timeout and
timer in this library was checked for work left behind:

- `Live.first_mid` races its read loop over a fresh `books` reader against `timeout`.
  Either way the reader is closed right after, which ends a read still pending with
  `` `Eof ``, so nothing keeps consuming the feed. `test_live_first_mid.ml` checks that
  no subscription stays open after a timeout and that a later call still answers.
- `Live.stop_signal ?run_for`: the alarm stays scheduled after a signal stops the run
  and fills an already full ivar when it fires; harmless.
- The poll timer and the summary timer stop with the loop (`stop` fills their `stop`
  ivar); a `stop` before the first summary leaves only the `after` alarm that would have
  started it.
- Venue calls (`place`, `cancel`, `open_orders`, `account`, `cancel_all`) have no
  timeout here: the adapters own them. A venue call that never answers blocks the
  sequencer, and `stop` waits for it.

## Allocation on the hot path

The strategy stays a persistent value (replay and shadow runs keep old states), so the
allocation work is inside each step:

- `Market_state.t` is one flat block: the variance and EWMA weight are unboxed
  `Float_u.t` fields, mid, microprice and the book are `or_null`. `on_book` allocates the
  new record (8 words) and nothing else; `mid_or_null`, `microprice_or_null`,
  `best_bid_or_null`, `best_ask_or_null`, `vol_bps_u` and `age_ms_or_null` are
  `[@@zero_alloc]`. The option accessors remain and allocate the `Some`.
- `Fx.price_to_float_u`, `size_to_float_u`, `price_of_float_u`, `shift_bps_u` and
  `bps_between_u` give the same results as the boxed conversions without boxing across
  the call (`[@@zero_alloc]`).
- `Quoter.quote` keeps the model terms in unboxed locals and conses each side's quotes in
  level order onto the list that follows, so it allocates only the result: 6 quotes are 48
  words. `For_testing.explain` keeps its per-rung records, built into arrays.
- `Risk.check_or_null` allocates nothing when the result is `Ok`.
- `Strategy.tick_u` returns an unboxed pair; a tick that sends nothing returns its
  argument and allocates nothing. `tick` boxes the pair (3 words). The live loop uses
  `tick_u`.

`test/test_allocation.ml` pins the words per call. `test/test_bisimulation.ml` runs the
library against the pure implementation as it was before this work (`test/ref_*.ml`): 300
random scripts of books, fills, acknowledgements, reconnects, ticks and parameter changes
must give the same plans and the same state sexp after every event, and 20,000 random
quoter cases the same quotes and unquoted reasons.

core_bench on an M-series Mac (arm64; CI is amd64), `bench/main.exe -quota 3`, time and
minor words per run (the bench closure adds 3 words to every row):

| Case | dev before | dev after | release before | release after | `-O3` before | `-O3` after |
| --- | ---: | ---: | ---: | ---: | ---: | ---: |
| `quote` flat | 470 ns, 357 w | 229 ns, 51 w | 332 ns, 357 w | 204 ns, 51 w | 227 ns, 287 w | 107 ns, 51 w |
| `quote` + `reconcile` | 1357 ns, 748 w | 1039 ns, 442 w | 1031 ns, 748 w | 862 ns, 442 w | 746 ns, 591 w | 624 ns, 355 w |
| `Market_state.on_book` | 40 ns, 42 w | 22 ns, 11 w | 28 ns, 42 w | 19 ns, 11 w | 6 ns, 5 w | 6 ns, 3 w |
| `tick`, nothing to send | 69 ns, 48 w | 58 ns, 6 w | 122 ns, 48 w | 49 ns, 6 w | 23 ns, 40 w | 16 ns, 3 w |
| `tick_u`, nothing to send | | 56 ns, 3 w | | 48 ns, 3 w | | 16 ns, 3 w |
| `on_book` + `tick`, requote | 447 ns, 290 w | 278 ns, 242 w | 389 ns, 290 w | 262 ns, 242 w | 250 ns, 260 w | 189 ns, 227 w |

Other agents shared the machine during these runs; differences under about 10% are noise,
words are exact. What is left on a requote is mostly `Reconciler.reconcile` (client order
ids and the plan list).

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

With `soft_max_position`, the room is also capped at `soft + bump_left - position` for
bids and `soft + bump_left + position` for asks: past the soft limit only the side that
would grow the position is pulled, where `max_position` halts. `bump_budget` is how far,
in total over a run, fills may carry the position past the soft limit; the strategy
tracks what it has used (`Strategy.bump_used`) and never refunds it.

A side without quotes gets a reason: `No_book`, `At_limit`, `Room_below_lot`,
`Non_positive_price` or `Soft_limit`.

### Reconciliation

Each desired quote keeps the closest resting order on the same side with exactly its size,
within `requote_threshold_bps` of its price and younger than `order_ttl_ms`, and that
stays strictly between the previous level's final price and the next desired price, so
the ladder never holds two orders on one price. Unmatched
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
| more than `max_actions_per_sec` cancels and places in the last second | sticky (optional cooldown) | same as position |
| `now - book_time > max_data_age_ms` | until data | cancel all |

The action rate is a rolling one-second window: a queue of (time, count) entries with
their running sum, trimmed from the front, amortised O(1) per plan. Two parameters shape
it:

- `action_rate_grace_ms` (default 5000): after a venue reconnect (`Live` calls
  `Strategy.on_resync`) plans are neither counted nor checked for this long, so the
  burst of cancels and places that follows a resync (for example after a simulator
  restart) cannot trip the switch. A burst that carries on past the grace period trips it
  within a few ticks.
- `action_rate_cooldown_ms` (default none): when set, a `Max_action_rate` halt clears
  this long after it tripped, the window is emptied and the strategy quotes again if the
  other checks pass. Position and drawdown halts stay permanent.

### Plausibility: reference guard and toxicity gate

Both are optional parameters, absent (and absent from a params sexp) by default; when
set they run after the risk checks pass and only ever pull quotes (`Pulled`, never
`Halted`).

`reference_guard` (`Reference_guard.Config`: `band_bps` 50, `grace_ms` 2s,
`max_jump_bps` none, `max_reference_age_ms` 30s, `stale_reference` `Pull`). The caller
feeds an independent reference mid (another venue, an index, a price-anchor mark) with
`Strategy.on_reference` or `Live.on_reference`. When the venue mid has been more than
`band_bps` from it for `grace_ms`, the strategy cancels everything
(`Pulled (Reference_divergence _)`) until the mid is back inside the band. A reference
update that jumps more than `max_jump_bps` skips the grace period. Without a fresh
reference, `Pull` pulls (`Reference_missing`) and `Ignore` quotes on. This is a check on
data that is fresh but wrong, next to the age check on data that stopped.

`toxicity_gate` (`Toxicity_gate.Config`: widen from 0.8, pull at 0.95, up to 5 bps more
half-spread, 2 bps of skew, sizes cut by up to half, signals valid 60s). The caller
feeds a signal in `[0, 1]` and the signed flow imbalance with `Strategy.on_toxicity` or
`Live.on_toxicity`; between the thresholds the quotes widen, move towards the side the
flow comes from (up when takers buy) and shrink, linearly in the signal; at `pull_at`
they are pulled (`Pulled (Toxic_flow _)`). A stale signal counts as none. mm-core does
not depend on market-data: the `mm-core-replay` package's `Toxicity_input` turns
market-data's VPIN buckets (`Market_data.Toxicity`, the percentile of VPIN against its own
history) into the signal, and `replay/test/test_toxicity_input.ml` runs a quiet market
then a run of buys through both. The percentile relaxes under one-sided flow that lasts
(VPIN stays at 1 and becomes the norm); a sustained toxic regime needs a threshold on VPIN
itself.

`Strategy.events_between ~before ~after ~now` (and `tick_with_events`) reports
`Reference (Diverged | Reference_jumped | Reference_missing | Resumed)` and
`Toxicity (Widening | Pulled | Cleared)`; `Live.on_strategy_event` registers a hook and
the loop logs each one. With neither configured nothing is computed and an idle tick still
allocates nothing (pinned with both configured too: 0 words, 79 ns against 67 ns without,
dev profile).

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
| max actions per second / grace after resync / cooldown | 100 / 5 s / none |
| soft max position / bump budget | none / 0 |

`Params.validate` names every field (no wildcard), so a new field needs its own check, and
rejects NaN and infinity in every float. Fields added after the first release
(`max_actions_per_sec`, `soft_max_position`, `bump_budget`, `action_rate_grace_ms`,
`action_rate_cooldown_ms`) default when absent from a sexp; the last two are also left
out of a printed sexp at their defaults.

`Params.for_instrument instrument ~mid` rescales these to another instrument: about 100
USD per order (nearest lot), 10 orders of position room and 1.5x that in notional. Pass
the venue's order value minimum as `~min_notional` (100 USD on Binance's BTCUSDT) and the
order size becomes at least 1.2x that, rounded up to the lot.

## Tests

`dune test` runs:

- expect and `Base_quickcheck` tests of the strategy: quotes never cross or breach the
  limit; reconciling a ladder against itself, or right after applying a plan, only keeps;
  `Risk.flatten` only sends reduce-only orders that shrink the position; NaN in any float
  parameter fails `validate`; quotes for one market state under each parameter preset;
  the soft limit and the action rate kill switch, its grace period and cooldown; the
  rolling window against a naive sum; hot parameter updates.
- live-loop tests over `test/memory_venue.ml`, an in-memory `Venue.S` that rejects
  crossing post-only orders and has a seeded bug mode (`Bugs.t`: random rejects, late
  cancel acks, fills split in parts, stale open-order polls, places that time out but rest
  later, slow place answers). A place that timed out and rests later is adopted, not
  duplicated; one that rests after the loop re-placed is cancelled.
- `test_live_params.ml`, on a synthetic clock: `update_params` widens the spread
  mid-run and the next requote moves only the outer quotes with no `cancel_all`; a
  requote burst right after a reconnect stays in the grace period, the same burst later
  trips the switch.
- a property test feeding random book, fill and wait scripts through `Live.Make` on a
  synthetic clock, checking after each step that resting quotes do not cross, sit on
  distinct prices, respect `max_position`, and that no order was rejected.
- `test/stepped.ml`, a deterministic stepped harness: a scripted book list, a toy
  exchange with seeded fills, and the strategy in lockstep; one run is pinned line by line.
- `test_allocation.ml` prints minor words per `Quoter.quote` call.

## Benchmarks

```sh
dune exec ./bench/main.exe -- -quota 2 -ascii
```

On an M-series Mac (default dev profile):

| Case | Time/run | Minor words/run |
| --- | --- | --- |
| `quote` flat | 362 ns | 357 |
| `quote` long | 363 ns | 357 |
| `quote` + `reconcile` (all kept) | 1125 ns | 748 |

Before the ladder kept per-rung terms for `explain` and reasons, `quote` was 317 ns and
221 words.
