open Syntax
open Dictionary

exception Wrong

type tree =
  | Leaf of { pair : lexeme }
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

(**let idList : (int * tree) list ref = ref []*)
let treelist : tree list ref = ref []

let newID =
  let count = ref 0 in
  fun () ->
    count := !count + 1;
    !count

let cat_extract (lex : lexeme) =
  String.sub (string_of_lexeme lex)
    (String.index_from (string_of_lexeme lex) 0 ' ')
    (String.index_from (string_of_lexeme lex)
       (String.index_from (string_of_lexeme lex) 0 ' ')
       ' ')

let word_extract (lex : lexeme) =
  String.sub (string_of_lexeme lex)
    (String.rindex_from (string_of_lexeme lex)
       (String.rindex_from (string_of_lexeme lex)
          (String.length (string_of_lexeme lex))
          ' ')
       ' ')
    (String.rindex_from (string_of_lexeme lex)
       (String.length (string_of_lexeme lex))
       ' ')

let rec make_constituent_node tree =
  match tree with
  | Leaf { pair } ->
      {
        category = cat_extract pair;
        children = L { category = cat_extract pair; child = word_extract pair };
      }
  | NodeOne { t; category = c; id } ->
      { category = c; children = C (make_constituent_node t) }
  | NodeTwo { t1; t2; category = c; id } -> (
      match t1 with
      | Leaf { pair } ->
          {
            category = c;
            children =
              LC
                ( { category = cat_extract pair; child = word_extract pair },
                  { category = c; children = C (make_constituent_node t2) } );
          }
      | _ -> (
          match t2 with
          | Leaf { pair } ->
              {
                category = c;
                children =
                  CL
                    ( { category = c; children = C (make_constituent_node t1) },
                      { category = cat_extract pair; child = word_extract pair }
                    );
              }
          | _ ->
              {
                category = c;
                children =
                  CC
                    ( { category = c; children = C (make_constituent_node t1) },
                      { category = c; children = C (make_constituent_node t2) }
                    );
              }))

(**idList := (id, node) :: !idList; ()*)
let add cat word syn =
  if check_lexeme syn { category = cat; child = word } then
    let node =
      NodeOne
        {
          t = Leaf { pair = create_lexeme cat word };
          category = cat;
          id = newID ();
        }
    in
    treelist := node :: !treelist
  else ()

let rec remove removal_id (lst : tree list) =
  match lst with
  | [] -> ()
  | h :: t -> (
      match h with
      | NodeOne { t = Leaf { pair }; category = c; id = i } ->
          if removal_id = i then treelist := t else remove removal_id t
      | NodeTwo { t1; t2; category; id = i } ->
          if removal_id = i then (
            treelist := t1 :: t;
            treelist := t2 :: !treelist)
          else remove removal_id t
      | _ -> raise Wrong)

let connect2 tree1 tree2 const syn =
  if
    check_constituent syn
      (make_constituent_node
         (NodeTwo { t1 = tree1; t2 = tree2; category = const; id = newID () }))
  then (
    treelist :=
      NodeTwo { t1 = tree1; t2 = tree2; category = const; id = newID () }
      :: !treelist;
    let () =
      match tree1 with
      | NodeOne { t; category; id } -> remove id !treelist
      | NodeTwo { t1; t2; category; id } -> remove id !treelist
      | _ -> raise Wrong
    in
    match tree2 with
    | NodeOne { t; category; id } -> remove id !treelist
    | NodeTwo { t1; t2; category; id } -> remove id !treelist
    | _ -> raise Wrong)
  else raise Wrong

let connect1 tree1 const syn =
  if
    check_constituent syn
      (make_constituent_node
         (NodeOne { t = tree1; category = const; id = newID () }))
  then (
    treelist :=
      NodeOne { t = tree1; category = const; id = newID () } :: !treelist;
    match tree1 with
    | NodeOne { t; category; id } -> remove id !treelist
    | NodeTwo { t1; t2; category; id } -> remove id !treelist
    | _ -> raise Wrong)
  else raise Wrong

let rec to_string tree =
  match tree with
  | Leaf { pair } -> "[" ^ string_of_lexeme pair ^ "]"
  | NodeOne { t; category } -> "[" ^ category ^ to_string t
  | NodeTwo { t1; t2; category } -> "[" ^ category ^ to_string t1 ^ to_string t2
