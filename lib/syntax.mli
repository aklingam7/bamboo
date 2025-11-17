exception SyntaxParsingFailure

type syntax_doc = string
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

val build_syntax : syntax_doc -> syntax
(** [build_syntax sd] evaluates to a value of type syntax representing the
    syntax defined by the text of [sd].

    Raises: [SyntaxParsingFailure] if the document is malformed. The syntax for
    the syntax is shown below (with \n representing the literal newline
    character and not an escape sequence).

    Syntax file syntax:
    syntax = 'lexical categories:\n', constituent list, 'constituent
     categories:\n', constituent list, 'rules:\n', ruleset, '\n' 
    constituent list = { constituent, '\n' } 
    ruleset = { rule, '\n' } 
    constituent = { regular character } 
    rule = { regular character }, ' -> ', { regular character } 
    regular character = ? all non newline or EOF character ?] *)

val check_lexeme : syntax -> lexical_node -> bool
(** [check_lexeme s l] returns true if and only if the lexeme l is of a valid
    lexical category that is defined in [s]. *)

val check_constituent : syntax -> constituent_node -> bool
(** [check_constituent s c] returns true if and only if the constituent [c] has
    a valid constituent category (that is defined in [s]) and its children are
    both valid as per the rules in ruleset for the constituent category of [c]
    and valid constiuents or lexemes themselves. *)
