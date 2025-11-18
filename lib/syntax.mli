exception InvalidSyntax
exception MalformedSyntax

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

val build_syntax : string -> syntax
(** [build_syntax sd] evaluates to a value of type syntax representing the
    syntax defined by the syntax description [sd]. See install.md for examples
    of valid values for [sd].

    Raises: [InvalidSyntax] if there are duplicates in the lexical or
    constituent categories. Note: extraneous categories, missing categories and
    duplicate rules in ruleset are accepted on purpose. Observe that they should
    always neither affect the behaviour of [check_lexeme] nor
    [check_constituent].

    Raises: [MalformedSyntax] if the document is malformed. The syntax for the
    syntax is shown below (with \n representing the literal newline character
    and not the escape sequence).

    Syntax file syntax:
    {@EBNF[
      syntax = 'lexical categories:\n', constituent list,
       'constituent categories:\n', constituent list, 'rules:\n', ruleset, '\n'
      constituent list = { constituent, '\n' }
      ruleset = { rule, '\n' }
      constituent = regular character, { regular character }
      rule = constituent , ' -> ', replacement
      replacement = constituent, [' ', constituent]
      regular character = ? all characters except U+0020 (space) ?
    ]} *)

val check_lexeme : syntax -> lexical_node -> bool
(** [check_lexeme s l] returns true if and only if the lexeme l is of a valid
    lexical category that is defined in [s]. *)

val check_constituent : syntax -> constituent_node -> bool
(** [check_constituent s c] returns true if and only if the constituent [c] has
    a valid constituent category (that is defined in [s]) and its children are
    both valid as per the rules in ruleset for the constituent category of [c]
    and valid constiuents or lexemes themselves. *)
