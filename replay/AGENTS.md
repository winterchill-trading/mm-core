# replay: mm-core replay library

`Replay` (in `replay/lib`) turns an event log from `Market_data.Replay_feed` into the strategy's inputs and
collects the plans. `replay/test` checks it against a checked-in day (`2026-01-01.bin`, the event log
fixture).

Rules:

- Do not regenerate the checked-in event log; a failing test means the strategy or the reader changed.
- The replay runs through a fresh strategy, so it shows what the strategy decides on the recorded input,
  not what the venue did.
