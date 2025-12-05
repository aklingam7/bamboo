open Tree

val to_string : tree -> string
(** [to_string t] returns a basic nested brackets representation of the tree
    workspace [t], showing all nodes. *)

val to_fancy_string : tree -> string
(** [to_fancy_string t] returns a fancy ASCII art representation of the tree
    workspace [t], showing all nodes. *)
