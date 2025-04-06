(lang dune 3.15)
(using mdx 0.4)
(using ctypes 0.3)

(name zfs)

(generate_opam_files true)

(source
 (github patricoferris/ocaml-zfs))

(authors "Patrick Ferris <patrick@sirref.org>")

(maintainers "Patrick Ferris <patrick@sirref.org>")

(license ISC)


(package
 (name zfs)
 (synopsis "libzfs bindings")
 (description "OCaml bindings to libzfs")
 (depends 
   ocaml 
   dune
   ctypes
   (mdx :with-test))
 (tags
  ("filesystem" "zfs")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
