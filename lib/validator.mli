open Syntax
open Tree
open Dictionary

(** Raised when validation fails *)
exception ValidationError of string

val validate_lexeme : string -> string -> syntax -> dictionary option -> bool
(** [validate_lexeme category word syn dict_opt] returns true if a lexeme with
    [category] and [word] is valid under syntax [syn] and, if [dict_opt] is
    [Some dict], exists in dictionary [dict]. If [dict_opt] is [None], only
    syntax validation is performed (all words are permitted). *)

val validate_constituent : tree_node -> string -> syntax -> bool
(** [validate_constituent node category syn] returns true if [node] viewed as a
    constituent of [category] is valid under syntax [syn]. *)