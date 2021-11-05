(lang dune 2.7)
(name uring)
(generate_opam_files true)
(formatting disabled)
(source (github ocaml-multicore/ocaml-uring))
(license "(ISC AND MIT)")
(authors "Anil Madhavapeddy" "Sadiq Jaffer" "Thomas Leonard")
(maintainers "anil@recoil.org")
(package
 (name uring)
 (synopsis "OCaml bindings for Linux io_uring")
 (description "Bindings to the Linux io_uring kernel IO interfaces. See https://github.com/ocaml-multicore/eio for a higher-level API using this.")
 (depends
  (cstruct (>= 6.0.1))
  (ocaml (>= 4.12.0))
  dune-configurator
  (lwt (and :with-test (>= 5.0.0)))
  (notty (and (>= 0.2.2) :with-test))
  (bechamel-notty (and (>= 0.1.0) :with-test))
  (bechamel (and (>= 0.1.0) :with-test))
  (logs (and :with-test (>= 0.5.0)))
  cmdliner
  (fmt (>= 0.8.10))
  bigstringaf
  (optint (>= 0.1.0))
  (alcotest (and (>= 1.4.0) :with-test))))
