# lib: mm-core library

Library `mm-core` (module `Mm_core`). Layers:

- Pure strategy: `Fx` (fixed-point helpers), `Params`, `Market_state` (flat record of `Float_u` fields
  with `or_null` accessors), `Inventory`, `Quote`, `Quoter`, `Action`, `Reconciler`, `Risk`,
  `Latency_histogram`, `Strategy`.
- Venue boundary: `Venue` (the `module type S` a client implements), `Live` (`Make (V : Venue.S)`:
  feeds, polls, sequenced plan execution, shutdown and the report).

Rules:

- `Strategy.update_params` keeps market state, inventory and resting orders. Symbol changes and invalid
  parameters are rejected, not applied.
- `Live.on_resync` runs after the loop's own grace period starts; `?on_venue` attaches observers before
  the loop starts.
- `Latency_histogram` reports two significant digits; keep it that way, the README quotes them.
