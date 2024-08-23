(lang dune 3.15)

(name riscv_disasm_from_sail)

(generate_opam_files true)

(source
 (github moste00/riscv_disasm_from_sail))

(authors "Mostafa Kamal")

(maintainers "Mostafa Kamal")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name riscv_disasm_from_sail)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune (libsail (= 0.17.1)))
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
