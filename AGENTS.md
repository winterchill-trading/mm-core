# mm-core: agent guide

## Purpose

The venue-neutral market-making core: a pure quoting strategy, a reconciler, risk checks, the
`Venue.S` client signature, and the `Live.Make` event loop that runs the strategy on any venue that
implements it. Nothing here talks to a venue; `market-maker` and the venue adapters do.

## Layout

| Path | Holds |
| --- | --- |
| `lib/` | Library `mm-core` (module `Mm_core`): `Params`, `Market_state`, `Inventory`, `Quoter`, `Reconciler`, `Risk`, `Toxicity_gate`, `Reference_guard`, `Strategy`, `Venue`, `Live` |
| `bin/` | The `mm-core` binary, package `mm-core-replay`: `replay -norm DIR -symbol S` prints the plans |
| `replay/` | The replay library (and `Toxicity_input`, market-data's VPIN as the toxicity gate's signal) and its tests, used by the binary |
| `bench/` | core_bench cases for `quote` and `reconcile` |
| `test/` | Expect and Base_quickcheck tests, bisimulation against the reference modules in `ref_*.ml`, live-loop tests over an in-memory venue, the stepped harness |
| `.github/` | CI workflow |

## Build, test, docs

Run from this repository, inside the workspace:

    opam exec --switch 5.2.0+ox -- dune build --root .. @mm-core/all
    opam exec --switch 5.2.0+ox -- dune build --root .. @mm-core/runtest

Several agents building at once need `DUNE_BUILD_DIR=_build-mm-core`.

Standalone, outside the workspace: add the organization's opam repository
(`opam repo add winterchill https://github.com/winterchill-trading/opam-repository.git`), install
the published dependencies, then run `dune build` here.

## Conventions

- Format this repository only: `dune build --root .. @mm-core/fmt --auto-promote`. Never
  `dune fmt` at the workspace root.
- Every module has an `.mli`; every public value has an odoc comment.
- The strategy is pure. A change to `Quoter` or `Reconciler` needs a bisimulation case in `test/` against
  the reference module, or an explanation why the reference does not cover it.
- Hot path: `Strategy.tick_u` returns an unboxed pair and allocates nothing on an idle tick. Pins on
  allocation are in `test/` and `bench/`. Word counts differ between arm64 and amd64, so a pin is a
  tolerance or zero, with a comment saying which.
- Parameters are validated by `Params.validate`, which names every field and rejects NaN and infinity.
  A new parameter defaults when absent from a sexp file.
- `Toxicity_gate` and `Reference_guard` only pull (`Pulled`), never halt, and run after the risk
  checks. New strategy fields are left out of its sexp while unused (`[@sexp.option]`,
  `[@sexp_drop_if]`), so the bisimulation against `test/ref_strategy.ml` keeps comparing states;
  keep that for any field the reference does not have.
- mm-core itself must not depend on market-data; code that needs it goes in `replay/`
  (package `mm-core-replay`).
- Comments for humans at pinned numbers, tolerances and the ladder-reconciliation fix.

## Dependencies

- Depends on sibling repositories `exchange-core` and `fixed-decimal`.
- Used by `hl-mm`, `binance-client`, `bybit-client`, `market-maker`.
- CI pin order is the `pins:` list in `.github/workflows/ci.yml`: `fixed-decimal`, then `exchange-core`.

## CI

`.github/workflows/ci.yml` runs the ox-setup action with the pins, then `dune build` and `dune test`.
The `obj-magic` job calls the reusable workflow in `winterchill-trading/infra`. To check a run, use
`gh run list --limit 1` and at most one `gh run watch <id> --exit-status`. Never poll in a loop.

## Pitfalls

- The action-rate kill switch is a rolling one-second window. `action_rate_grace_ms` suspends it after a
  reconnect; position and drawdown halts stay permanent.
- The reconciler once kept a resting order at the next ladder level's price. It keeps an order only if it
  lies strictly between the previous level's final price and the next desired price; the live-loop
  property test caught it.
- Venue calls have no timeout here. A venue that never answers blocks the loop and `stop`.
- One symbol per `Live.Make` run.
- The toxicity gate's default thresholds are for a percentile signal. The percentile relaxes under
  one-sided flow that lasts; a sustained regime needs a threshold on VPIN itself.
- `Live.stop` on a synthetic time source waits on its timers: advance the clock after calling it, or
  the test hangs.
- The position is not flattened on exit; `stop` only cancels resting orders.

## Where to read more

- `README.md` (modules, venue signature, live loop, hot parameters, replay, allocation, strategy, tests)
- `../specs/repos/mm-core.md` (design summary, known gaps)
- `../specs/design/market-making.md`
- `../oxcaml-claude-skills/AGENTS.md`, `../oxcaml-claude-skills/GOTCHAS.md`
- Skill `oxcaml-performance` in `../oxcaml-claude-skills/skills/`

## Never do

- Add a venue client or a network call to this repository; adapters live in their own repositories.
- Run `opam install`, `upgrade` or `pin` that would change `ocaml-variants`, `core`, `base`, `async` or
  `ppx_jane` on the shared `5.2.0+ox` switch.
- Publish, tag or release.
- Poll CI in a loop.
