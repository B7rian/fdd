(lang dune 3.7)

(name fdd)

(generate_opam_files true)

;(source
; (github username/reponame))
;(license LICENSE)
; (documentation https://url/to/documentation)

(authors "Brian Hughes")
(maintainers "Brian Hughes")

(package
 (name fdd)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends (ocaml (>= 5.3.0)) dune sha ppx_expect cmdliner)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
