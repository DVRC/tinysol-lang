(lang dune 3.19)

(name tinysol-lang)

(generate_opam_files true)

(source
 (github dvrc/tinysol-lang))

(authors
	"Davide Ricco <d.ricco@estudenti.unica.it>"
	"Luca Lampis <f.meola@studenti.unica.it>"
	"Francesco Meola <l.lampis18@studenti.unica.it"
)

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name tinysol-lang)
 (synopsis "Subset implementation of Solidity, project for LIP class")
 (description "A subset implementation of the Solidity language implemented
 in OCaml, for the programming languages theory class of UniCa")
 (depends ocaml)
 (tags
  ("add topics" "to describe" your project)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
