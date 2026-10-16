# Changes

## Unreleased (next patch: 0.1.1)

Additive for code that builds `Params.t` from `Params.btc` or `Params.for_instrument` and
reads params sexps; a record literal listing every `Params.t` field needs the two new
fields.

- `Toxicity_gate`: widen, skew or pull quotes on a toxicity signal; `Params.toxicity_gate`
  (absent: off).
- `Reference_guard`: pull quotes while the venue mid strays from a reference mid past a
  grace period, or at once after a reference jump; typed events;
  `Params.reference_guard` (absent: off).
- `Risk.Reason` gains `Reference_divergence`, `Reference_missing` and `Toxic_flow`, all
  non-sticky. Exhaustive matches on `Risk.Reason.t` elsewhere need the new cases.
- `Strategy.on_reference`, `on_toxicity`, `toxicity_decision`, `reference_guard`,
  `Event`, `events_between`, `tick_with_events`. The strategy's sexp is unchanged while
  neither is configured.
- `Quoter.quote`, `quote_with_reasons`, `unquoted`, `model` and `For_testing.explain` take
  an optional `?shift_bps` reservation shift. At 0 (the default) the quotes are the same
  floats as before, which the bisimulation against `test/ref_quoter.ml` still checks; the
  shift itself has its own expect test, since the reference has no shift.
- `Live.Make`: `on_reference`, `on_toxicity`, `on_strategy_event`.
- `mm-core-replay`: `Toxicity_input`, market-data's VPIN buckets as the gate's signal.
- A requote tick allocates 3061 words (was 3050): the strategy record has four more
  fields. An idle tick still allocates nothing, with or without the guard and gate.

## 0.1.0

Not yet in the organization's opam repository.
