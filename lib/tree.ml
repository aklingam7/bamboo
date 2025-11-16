open Syntax
open Dictionary

exception Wrong

type tree =
  | Leaf of { lexeme : string }
  | NodeOne of {
      t : tree;
      category : string;
    }
  | NodeTwo of {
      t1 : tree;
      t2 : tree;
      category : string;
    }

let treelist : tree list = []

let add cat word =
  NodeOne { t = Leaf { lexeme = word }; category = cat } :: treelist

let rec remove cat word = function
  | [] -> treelist
  | h :: t -> (
      match h with
      | NodeOne { t = Leaf { lexeme }; category = c } ->
          if lexeme = word && cat = c then t else h :: remove cat word t
      | _ -> raise Wrong)

let connect tree1 tree2 const =
  NodeTwo { t1 = tree1; t2 = tree2; category = const }

let print t = failwith "not implemented"

let rec validate t =
  match t with
  | Leaf str -> true
  | NodeOne { t; category } -> validate t
  | NodeTwo { t1; t2; category } -> validate t1 && validate t2
