(* TODO: Add functionality for all below comments*)

(* Creates empty syntax tree *)
(* Adds a lexeme node *)
(* Connects two nodes *)
(* Removes a node *)
(* Prints the tree *)
(* Validates the tree *)
open Syntax
open Dictionary

type tree

exception Wrong

val treelist : tree list

val add : string -> string -> tree list
(**[add cat word] connects a word and its syntactic category to its <type>.
   Requires [cat] is a string represeting a valid syntactic category and [word]
   is a string of that syntactic category. Raises [] if category and word do not
   match.*)

val remove : string -> string -> tree list -> tree list
(**[remove cat word] removes a word and its syntactic category from the <type>*)
val connect : tree -> tree -> string -> tree
(**[connect t1 t2] connects two nodes together to form a larger node.*)
val print : tree -> unit
(**[print t] prints tree [t1] to output. Requires [t1] is a tree.*)
val validate : tree -> bool
(**[validate t] checks whether [t] is a valid tree.*)

