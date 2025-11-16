type syntax

type lexical_node = {
  category : string;
  child : string;
}

type constituent_children =
  | CC of (constituent_node * constituent_node)
  | CL of (constituent_node * lexical_node)
  | LC of (lexical_node * constituent_node)
  | C of constituent_node
  | L of lexical_node

and constituent_node = {
  category : string;
  children : constituent_children;
}

val check_lexeme : syntax -> lexical_node -> bool
(** [check_lexeme s l] returns true if and only if the lexeme l is of a valid
    lexical category that is defined in [s]. *)

val check_constituent : syntax -> constituent_node -> bool
(** [check_constituent s c] returns true if and only if the constituent [c] has
    a valid constituent category (that is defined in [s]) and its children are
    valid as per the ruleset of syntax [s].

    Representation invariant: any constituent or lexical nodes that are used to
    construct [c] should themselves be valid (i.e., their respective check
    function returns true). *)
