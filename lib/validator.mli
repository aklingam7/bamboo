open Syntax
open Tree

(** Raised when validation fails *)
exception ValidationError of string

val as_constituent : tree -> string -> constituent_node
(** [as_constituent node category] views [node] as a constituent_node with the
    given [category] for syntax validation. This handles the structural
    transformation between Tree's representation and Syntax's representation. *)