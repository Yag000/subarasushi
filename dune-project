(lang dune 3.10)

(name subarasushi)

(generate_opam_files true)

(source
 (uri git+https://gaufre.informatique.univ-paris-diderot.fr/iglesias/subarasushi))

(authors "Yago Iglesias Vazquez, Yanis Lacenne, Tony Ly Soan, Nathan Guetteville")

(maintainers "Yago Iglesias Vazquez, Yanis Lacenne, Tony Ly Soan, Nathan Guetteville")

(license MIT)

(package
 (name subarasushi)
 (synopsis "Sushi Go Party implementation in OCaml")
 (description "Sushi Go Party implementation in OCaml")
 (depends ocaml dune ppx_inline_test ppx_expect ppx_deriving alcotest qcheck qcheck-alcotest))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
