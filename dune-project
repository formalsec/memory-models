(lang dune 3.0)

(generate_opam_files true)

(source
 (github formalsec/memory-models))

(authors
 "André Mendes <andreman333@gmail.com>"
 "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(maintainers "Filipe Marques <filipe.s.marques@tecnico.ulisboa.pt>")

(license "GPL-3.0-only")

(package
 (name mm-wasm)
 (synopsis "Memory models for Wasm")
 (description "Symboic data structures to model Wasm's linear memory")
 (depends
  dune
  (odoc :with-doc)
  (ocaml (>= "4.14.0"))
  (encoding (>= "0.0.3"))))
