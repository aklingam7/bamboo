To install the program, download all the files and extract them into their own directory.

Then, navigate to the root of the project.

First, create an OPAM switch with OCaml 5.2.0 (recommended) by running the following command:

`opam switch create bamboo-project 5.2.0`

`eval $(opam env)`

Then, install the required dependencies:

`opam install ocaml dune csv ansiterminal batteries ounit2 ppx_string ppx_inline_test bisect_ppx`

Then, run `dune build`. 

Then, you can run the program with: 

`dune exec bin/main.exe -- -s data/syntax.txt -d data/dict.csv`

(replacing the syntax and dicitonary files with any appropriately formatted files you create, and optionally not adding a dictionary file)
