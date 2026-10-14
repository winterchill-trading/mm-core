# .github: mm-core CI

`workflows/ci.yml` runs the ox-setup action with the pins `fixed-decimal` and `exchange-core`, then
`dune build` and `dune test`.

Rules:

- Keep the pin order in step with `dune-project` and `../specs/repos/mm-core.md`.
- To check a run: `gh run list --limit 1`, then at most one `gh run watch <id> --exit-status`.
