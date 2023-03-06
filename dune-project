(lang dune 3.6)

(name derivation)

(generate_opam_files true)

(source
 (github Robotechnic/derivation))

(authors "Robotechnic")

(maintainers "Robotechnic")


(package
 (name derivation)
 (synopsis "An OCaml program to derivate functions")
 (description "With this program, you can derivate functions given in arguments")
 (depends ocaml dune)
 (tags
  (topics OCaml math derivation parser)))
