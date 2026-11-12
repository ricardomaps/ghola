(lang dune 3.23)

(name ghola)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Ricardo Mapurunga Junior <ricardomapurungajunior@proton.me>")

(maintainers "Ricardo Mapurunga Junior <ricardomapurungajunior@proton.me>")

(license LICENSE)

(package
 (name ghola)
 (synopsis "Window manager for the River compositor")
 (depends ocaml containers wayland xkbcommon xdg)
 (tags
  ("add topics" "to describe" your project)))
