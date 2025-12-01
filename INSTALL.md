To install the program, download all the files and extract them into their own directory. 

To install all dependencies, first navigate to the root of the project, and then run the following commands:

`opam install ocaml dune csv batteries ounit2 ppx_string`

Then, run `dune build`. 

Then, you can run the program with: 

`dune exec bin/main.exe -- -s data/syntax.txt -d data/dict.csv`

(replacing the syntax and dicitonary files with any appropriately formatted files you create)

Syntax trees represent the ways to visualize sentence structures. The top of a tree, the root, typically contains a TP, which represents the sentence as a whole. The rule, TP -> DP VP, indicates that a TP node branches into two nodes, a DP and VP. The visualization is shown below,
   TP
   / \
  DP VP.

A DP represents a noun phrase with a determiner (i.e, "the", "my", "his", "one", "four" etc.), while a VP represents a verb phrase. For the sentence, "The boy walks the dog", the simple structure is shown below,

       TP
     /    \
    DP    VP
  /   \  /   \
  D   NP V     DP
  |   |  |    /  \
  The N walks D  NP
      |       |   |
     boy      the N
                  |
                  dog

It is important to note that we can not connect two lexical nodes together (i.e, N, V, D, P, etc.). For example, for a DP to be formed, we must first form an NP first. We can not connect a D with an N as those are both lexical nodes.

Right now, to connect nodes, use the rightmost id number as that represents the id number of the topmost node of the subtree.

Here are the lexical node representations:
N-Noun
V-Verb
D-Determiner
P-Preposition (not originally given)
Adj-Adjective

Here are some general nodes that are not originally given:
PP-> P DP
VP->V PP
AdjP->Adj
NP-> AdjP N