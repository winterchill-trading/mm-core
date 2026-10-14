# bin: mm-core replay binary

`main.ml` is the `mm-core` executable from the `mm-core-replay` package. `replay -norm DIR -symbol S`
reads a `Replay_feed` event log and prints the plans `Strategy` produces. No fills are simulated, so the
output shows decisions, not P&L.

Rules:

- Keep the output stable enough to diff between runs.
- It reads files only; it opens no network connection.
