(lang dune 3.4)

(using menhir 2.1)

(name kyoumi_lang)

(generate_opam_files true)

(source
 (github EruEri/kyoumi-lang))

(authors "EruEri")

(maintainers "EruEri")

(license GPL-3.0)

(documentation https://url/to/documentation)

(package
 (name kyoumi_lang)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
