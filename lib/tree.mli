open Syntax
open Dictionary

(** [tree] represents an individual node in a syntax tree. This type is
    transparent to allow pattern matching for visualization and analysis.

    - [Leaf] contains a lexeme (word + category) and unique ID
    - [NodeOne] is a constituent with one child and unique ID
    - [NodeTwo] is a constituent with two children and unique ID

    Note: The module represents a workspace containing multiple disconnected
    trees (a forest), managed via the treelist reference. Nodes should only be
    constructed via the functions in this module to ensure validity. *)
type tree =
  | Leaf of {
      pair : lexeme;
      id : int;
    }
  | NodeOne of {
      t : tree;
      category : string;
      id : int;
    }
  | NodeTwo of {
      t1 : tree;
      t2 : tree;
      category : string;
      id : int;
    }

exception Wrong

val treelist : tree list ref

val add : string -> string -> syntax -> dictionary -> tree list ref -> unit
(** [add category word syntax dictionary] connects a word and its syntactic
    category to its <type>. Requires [category] is a string represeting a valid
    syntactic category of [syntax] and [word] is matched to that syntactic
    category in [dictionary]. Raises [] if category and word do not match. *)

val get : int -> tree list -> tree
(** [get id treelist] returns the node associated with the given id in
    [treelist]. *)

val remove : int -> tree list ref -> unit
(** [remove id treelist] removes the node associated with the given id from
    [treelist]. *)

val connect2 : tree -> tree -> string -> syntax -> tree list ref -> unit
(** [connect t1 t2] adds a constituent node parenting the two given nodes (in
    order) to the syntax tree, and removes their children from the toplevel of
    treelist. *)

val connect1 : tree -> string -> syntax -> tree list ref -> unit
(** [connect t1] adds a constituent node parenting the given node to the syntax
    tree, and removes its children from the toplevel of treelist. *)

val to_string : tree -> string
(** [to_string tree] returns a string representation of [tree]. *)

val to_fancy_string : tree -> string
(** [to_fancy_string tree] returns a fancy string representation of [tree]. *)

val to_fancy_string_list : tree list -> string
(** [to_fancy_string_list trees] returns a fancy string representation of each
    tree in [trees] placed horizontally alongside one another. *)
