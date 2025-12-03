open Syntax
open Tree
open Dictionary

(** Raised when validation fails *)
exception ValidationError of string

val validate_lexeme : string -> string -> syntax -> dictionary -> bool
(** [validate_lexeme category word syn dict] returns true if a lexeme with
    [category] and [word] is valid under syntax [syn] and exists in dictionary
    [dict]. *)

val validate_constituent : tree_node -> string -> syntax -> bool
(** [validate_constituent node category syn] returns true if [node] viewed as a
    constituent of [category] is valid under syntax [syn]. *)