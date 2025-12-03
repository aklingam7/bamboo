open Tree

val to_string : tree_node -> string
(** [to_string node] returns a string representation of [node]. *)

val to_fancy_string : tree -> string
(** [to_fancy_string t] returns a fancy ASCII art representation of the tree
    workspace [t], showing all top-level nodes. *)

val to_fancy_string_list : tree_node list -> string
(** [to_fancy_string_list nodes] returns a fancy string representation of all
    nodes, placed horizontally alongside one another. *)
