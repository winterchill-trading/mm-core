(lang dune 3.17)

(name mm-core)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/mm-core))

(package
 (name mm-core)
 (synopsis "Venue-neutral market-making core")
 (description
  "Avellaneda-Stoikov style quoting, reconciliation, risk and inventory on exchange-core types, with an Async live loop parameterised over a venue client signature.")
 (depends
  ocaml
  dune
  core
  async
  exchange-core
  fixed-decimal
  ppx_jane
  (core_unix :with-test)
  (core_bench :with-test)))
