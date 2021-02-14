(lang dune 2.7)
(name uring)
(generate_opam_files true)
(source (github ocaml-multicore/ocaml-uring))
(license ISC)
(authors "Anil Madhavapeddy" "Sadiq Jaffer")
(maintainers "anil@recoil.org")
(package
 (name uring)
 (synopsis "OCaml bindings for Linux io_uring")
 (description "Bindings to the Linux io_uring kernel IO interfaces.")
 (depends
  (lwt (>= 5.0.0))
  (notty (and (>= 0.2.2) :with-test))
  (bos (and (>= 0.2.0) :with-test))
  (bechamel-notty (and (>= 0.1.0) :with-test))
  (bechamel (and (>= 0.1.0) :with-test))
  (logs (>= 0.5.0))
  cmdliner
  (fmt (>= 0.8.4))
  bigstringaf
  (alcotest :with-test)))
