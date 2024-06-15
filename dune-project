(lang dune 3.15)

(name riscv_disasm_from_sail)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Mostafa Kamal")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name riscv_disasm_from_sail)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune libsail)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
