# test: mm-core tests

- `ref_*.ml`: pure reference modules. `test_bisimulation.ml` runs 300 random event scripts against them.
- `test_live*.ml` and `memory_venue.ml`: the live loop over an in-memory `Venue.S`, with a seeded bug mode
  (random rejects, late cancel acks, split fills, stale polls, timeouts that rest later).
- `stepped.ml`: a deterministic stepped harness with a toy exchange; one run is pinned line by line. A
  change to its output is a behaviour change, not a test update.
- `test_allocation.ml`: allocation pins on the hot path; read the tolerance comment before editing it.
- Use the synthetic `Time_source` for live-loop tests; do not sleep.
