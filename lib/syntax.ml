exception SyntaxParsingFailure

type syntax_doc = string

type syntax = unit (* placeholder *)
(** This implementation is currently unimplimented. *)

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

let build_syntax (sd : syntax_doc) = failwith "unimplimented"

let check_lexeme (s : syntax) (l : lexical_node) : bool =
  failwith "unimplimented"

let check_constituent (s : syntax) (c : constituent_node) : bool =
  failwith "unimplemented"
