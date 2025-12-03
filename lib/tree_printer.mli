open Tree

val to_string : tree -> string
(** [to_string node] returns a string representation of [node]. *)

val to_fancy_string : tree -> string
(** [to_fancy_string node] returns a fancy ASCII art representation of [node]. *)

val to_fancy_string_list : tree list -> string
(** [to_fancy_string_list nodes] returns a fancy string representation of all
    nodes, placed horizontally alongside one another. *)
