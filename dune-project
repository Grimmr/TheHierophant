(lang dune 3.6)

(name TheHierophant)

(generate_opam_files true)

;(source (github username/reponame))

(authors "Grimmr")

(maintainers "Grimmr")

;(license LICENSE)

;(documentation https://url/to/documentation)

(package
 (name TheHierophant)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 ;(tags (topics "to describe" your project))
 )

(using menhir 2.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
