# Bamboo

A terminal syntax-tree builder for X-bar theory, written in OCaml. Bamboo lets you build constituency trees interactively while a user-defined grammar (and optionally a dictionary) checks every node you add, and renders the result as ASCII art or nested brackets. Team project for CS 3110, Functional Programming and Data Structures, Cornell University, Fall 2025.

**Team**

- Prajwal Reddy (pmr93)
- Alex Doboli (add82)
- Aditya Lingam (akl84)
- Dashiell Yamanishi (dly23)

Mirror of the team repository at `github.coecis.cornell.edu/dly23/PANDA-CS-3110-Team-Project` (Cornell login required). See `AUTHORS.md` for mentor, grader, and GenAI-usage disclosure.

## Features

- Grammar as data: lexical categories, constituent categories, and production rules are read from a plain-text syntax file, so the tool is not tied to one theory of syntax.
- Optional dictionary: a CSV of (category, word) pairs restricts which lexemes may be added. The included `data/dict.csv` has ~3,200 entries.
- Validation on every step: adding a lexeme or connecting nodes is rejected if it violates the grammar or dictionary.
- Two renderers: ASCII-art trees and nested-bracket notation, toggled at runtime.
- Live extension: new words and new grammar rules can be added from the REPL and are written back to the files you loaded.
- Colour-coded REPL feedback (ok / warning / error) via ANSITerminal.
- OUnit test suite with inline tests and bisect_ppx coverage instrumentation.

## Running

```
opam switch create bamboo-project 5.2.0
eval $(opam env)
opam install dune csv ansiterminal batteries ounit2 ppx_string ppx_inline_test bisect_ppx
dune build
dune exec bin/main.exe -- -s data/syntax.txt -d data/dict.csv
```

`-s` sets the syntax file, `-d` the dictionary. Omitting `-d` disables word checking (any word is permitted). Omitting `-s` starts with an empty grammar. Run the tests with `dune test`.

## Commands

Arguments with spaces go in double quotes.

| Command | Effect |
|---------|--------|
| `add <category> <word>` | Add a lexical leaf, e.g. `add N boy` |
| `connect <category> <id>` | Wrap one node in a unary constituent, e.g. `connect NP 1` |
| `connect <category> <id1> <id2>` | Join two nodes under a binary constituent, e.g. `connect DP 4 3` |
| `remove <id>` | Remove a top-level node and everything that references it |
| `syntax` | Print the current grammar |
| `expand-dict <category> <word>` | Add a word to the dictionary and its CSV file |
| `expand-syntax <section> "<entry>"` | Add to the grammar; section is `lexical-categories`, `constituent-categories`, or `rules`, e.g. `expand-syntax rules "PP -> P DP"` |
| `toggle-fancy` | Switch between ASCII-art and nested-bracket output |
| `reset-tree` | Clear the workspace |
| `review` | Reprint the tutorial text |
| `quit` | Exit |

Nodes are referred to by the id shown in the display. To connect a subtree, use the id of its topmost node (the rightmost id).

## Syntax file format

```
lexical categories:
N
V
D
constituent categories:
DP
VP
TP
NP
rules:
TP -> DP VP
NP -> N
DP -> D NP
VP -> V DP
```

Each rule has one or two symbols on the right-hand side. The formal grammar for this file is in the doc comment on `Syntax.build_syntax` in `lib/syntax.mli`. The dictionary is a two-column CSV, `category,word`, one lexeme per line.

## Background

Syntax trees are a way to visualize sentence structure. The root of a tree is typically a TP, which represents the sentence as a whole. The rule `TP -> DP VP` says that a TP node branches into two children, a DP and a VP:

```
  TP
  / \
 DP  VP
```

A DP is a noun phrase headed by a determiner (e.g. "the", "my", "his", "one", "four"); a VP is a verb phrase. The structure of "The boy walks", as Bamboo renders it:

```
       [TP] (7)
          / \
        /     \
      /         \
      |           \
   [DP] (5)    [VP] (6)
     / \          |
   /     \      V (2)
D (4) [NP] (3)    |
  |      |      walks
 the   N (1)
         |
        boy
```

Two lexical nodes cannot be connected directly. To form a DP, for example, you must first form an NP; connecting a D straight to an N is not allowed, since both are lexical.

| Label | Category |
|-------|----------|
| N     | Noun |
| V     | Verb |
| D     | Determiner |
| P     | Preposition |
| Adj   | Adjective |

## Project layout

```
bin/main.ml           REPL, argument parsing, command dispatch
lib/syntax.ml         grammar parsing and rule checking
lib/dictionary.ml     lexeme and dictionary types
lib/tree.ml           tree workspace: add, connect, remove, ids
lib/tree_printer.ml   ASCII-art and bracket renderers
lib/validator.ml      ties syntax and dictionary checks together
test/                 OUnit tests, one file per module
data/                 sample syntax file and dictionary
```
