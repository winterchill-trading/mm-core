# bench: mm-core benchmarks

core_bench cases for `quote` (flat and long) and `quote` plus `reconcile` with all orders kept. The
README table holds the figures from an arm64 Mac in the dev profile.

Rules:

- Quote the machine with every new figure. Word counts are not portable across arm64 and amd64.
- A change that moves a figure updates the README table in the same commit.
- Benchmarks do not run in CI.
