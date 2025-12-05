open Dictionary

type tree
(** [tree] represents a syntax tree construction workspace for X-bar theory.
    This type encapsulates the forest of disconnected syntax trees being built,
    along with ID generation state. It is abstract to ensure proper management
    of node IDs and tree structure. *)

(** [tree_node] represents an individual node in a syntax tree. This type is
    transparent to allow pattern matching for visualization and analysis.

    - [Leaf] contains a lexeme (word + category) and unique ID
    - [NodeOne] is a constituent with one child and unique ID
    - [NodeTwo] is a constituent with two children and unique ID *)
type tree_node =
  | Leaf of {
      pair : lexeme;
      id : int;
    }
  | NodeOne of {
      t : tree_node;
      category : string;
      id : int;
    }
  | NodeTwo of {
      t1 : tree_node;
      t2 : tree_node;
      category : string;
      id : int;
    }

exception NonexistentNodeID of int
exception NonTopLevelNodeID of int

val empty : tree
(** [empty] is an empty syntax tree workspace with no nodes. *)

val add : tree -> lexeme -> tree * int
(** [add t lexeme] is [(t', id)] where [t'] is the tree with a new orphan lexeme
    node added and [id] is its unique identifier. *)

val get : tree -> int -> tree_node
(** [get t id] returns the tree_node with identifier [id] in [t].

    Raises: [NonexistentNodeID] if no node has that id. *)

val remove : tree -> int -> tree
(** [remove t id] is the tree with the node of [id] removed along with any nodes
    that reference it.

    Raises: [NonTopLevelNodeID] if the node doesn't exist. *)

val connect2 : tree -> tree_node -> tree_node -> string -> tree * int
(** [connect2 t n1 n2 category] is [(t', id)] where [t'] contains a new
    constituent of [category] parenting nodes [n1] and [n2], and [id] is the
    constituent's ID. *)

val connect1 : tree -> tree_node -> string -> tree * int
(** [connect1 t n category] is [(t', id)] where [t'] contains a new constituent
    of [category] parenting node [n], and [id] is the constituent's ID. *)

val to_list : tree -> tree_node list
(** [to_list t] is a list of all top-level (orphan) tree nodes in [t]. *)

val string_of_tree_node : tree_node -> string
(** [string_of_tree_node str] is a string representation of a tree node. *)
