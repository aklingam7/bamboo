To install the program, download all the files and extract them into their own directory. 

To install all dependencies, first navigate to the root of the project, and then run the following commands:

`opam install ocaml dune csv batteries ounit2 ppx_string`

Then, run `dune build`. 

Then, you can run the program with: 

`dune exec bin/main.exe -- -s data/syntax.txt -d data/dict.csv`

(replacing the syntax and dicitonary files with any appropriately formatted files you create)