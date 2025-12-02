open Syntax
open Dictionary

type tree

exception Wrong

val treelist : tree list ref

val add : string -> string -> syntax -> dictionary -> unit
(** [add category word syntax dictionary] connects a word and its syntactic
    category to its <type>. Requires [category] is a string represeting a valid
    syntactic category of [syntax] and [word] is matched to that syntactic
    category in [dictionary]. Raises [] if category and word do not match. *)

val get : int -> tree list -> tree
(** [get id treelist] returns the node associated with the given id in
    [treelist]. *)

val remove : int -> tree list -> unit
(** [remove id treelist] removes the node associated with the given id from
    [treelist]. *)

val connect2 : tree -> tree -> string -> syntax -> unit
(** [connect t1 t2] adds a constituent node parenting the two given nodes (in
    order) to the syntax tree, and removes their children from the toplevel of
    treelist. *)

val connect1 : tree -> string -> syntax -> unit
(** [connect t1] adds a constituent node parenting the given node to the syntax
    tree, and removes its children from the toplevel of treelist. *)

val to_string : tree -> string
(** [to_string tree] returns a string representation of [tree]. *)

val to_fancy_string : tree -> string
(** [to_fancy_string tree] returns a fancy string representation of [tree]. *)

val to_fancy_string_list : tree list -> string
(** [to_fancy_string_list trees] returns a fancy string representation of each
    tree in [trees] placed horizontally alongside one another. *)
