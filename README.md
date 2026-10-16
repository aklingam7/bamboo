# Bamboo

An interactive syntax-tree builder written in OCaml. Team project for CS 3110, Functional Programming and Data Structures, Cornell University, Fall 2025.

**Team**

- Prajwal Reddy (pmr93)
- Alex Doboli (add82)
- Aditya Lingam (akl84)
- Dashiell Yamanishi (dly23)

Mirror of the team repository at `github.coecis.cornell.edu/dly23/PANDA-CS-3110-Team-Project` (Cornell login required).

## Background

Syntax trees are a way to visualize sentence structure. The root of a tree is typically a TP, which represents the sentence as a whole. The rule `TP -> DP VP` says that a TP node branches into two children, a DP and a VP:

```
  TP
  / \
 DP  VP
```

A DP is a noun phrase headed by a determiner (e.g. "the", "my", "his", "one", "four"); a VP is a verb phrase. The structure of "The boy walks" is:

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

## Lexical nodes

| Label | Category |
|-------|----------|
| N     | Noun |
| V     | Verb |
| D     | Determiner |
| P     | Preposition (added beyond the original spec) |
| Adj   | Adjective |

## Building trees

Two lexical nodes cannot be connected directly. To form a DP, for example, you must first form an NP; connecting a D straight to an N is not allowed, since both are lexical.

To connect nodes, refer to a subtree by the id of its topmost node, which is the rightmost id shown in the display.
