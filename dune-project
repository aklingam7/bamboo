(lang dune 3.20)

(name bamboo)

(generate_opam_files true)

(source
 (uri "git@github.coecis.cornell.edu:dly23/PANDA-CS-3110-Team-Project.git"))

(authors
 "Aditya Lingam <akl84>"
 "Alex Doboli <add82>"
 "Dashiell Yamanishi <dly23>"
 "Prajwal Reddy <pmr93>")

(package
 (name bamboo)
 (synopsis "A syntax tree builder")
 (description "A tool to allow syntacticians to construct ASCII and nested brackets trees while staying within set dictionaries and syntaxes, while also allowing them to modify those restrictions if necessary.")
 (depends ocaml ppx_string)
 (tags ("syntax")))
