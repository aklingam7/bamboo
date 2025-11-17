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

val treelist : tree list ref

val add : syntax_doc -> syntax_doc -> syntax -> unit
(**[add cat word] connects a word and its syntactic category to its <type>.
   Requires [cat] is a string represeting a valid syntactic category and [word]
   is a string of that syntactic category. Raises [] if category and word do not
   match.*)

val remove : int -> tree list -> unit
(**[remove cat word] removes a word and its syntactic category from the <type>*)
val connect2 : tree -> tree -> syntax_doc -> syntax ->unit
(**[connect t1 t2] connects two nodes together to form a larger node.*)
val connect1 : tree -> syntax_doc -> syntax->unit
(**[connect t1] connects one node to a constituent together to form a larger node.*)
val to_string : tree -> string
(**[print t] prints tree [t1] to output. Requires [t1] is a tree.*)

