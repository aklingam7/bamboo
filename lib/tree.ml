open Syntax
open Dictionary

exception Wrong

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

(**let idList : (int * tree) list ref = ref []*)
let treelist : tree list ref = ref []

let newID =
  let count = ref 0 in
  fun () ->
    count := !count + 1;
    !count

let rec make_constituent_node tree =
  match tree with
  | Leaf { pair } ->
      {
        category = get_lexeme_category pair;
        children =
          L
            {
              category = get_lexeme_category pair;
              child = get_lexeme_word pair;
            };
      }
  | NodeOne { t; category } -> (
      match t with
      | Leaf { pair } ->
          let c = make_constituent_node t in
          {
            category;
            children = L { category = c.category; child = get_lexeme_word pair };
          }
      | _ ->
          let c = make_constituent_node t in
          { category; children = C c })
  | NodeTwo { t1; t2; category } -> (
      let c1 = make_constituent_node t1 in
      let c2 = make_constituent_node t2 in
      match (t1, t2) with
      | Leaf { pair }, _ ->
          {
            category;
            children =
              LC
                ( {
                    category = get_lexeme_category pair;
                    child = get_lexeme_word pair;
                  },
                  c2 );
          }
      | _, Leaf { pair } ->
          {
            category;
            children =
              CL
                ( c1,
                  {
                    category = get_lexeme_category pair;
                    child = get_lexeme_word pair;
                  } );
          }
      | _ -> { category; children = CC (c1, c2) })

let add cat word syn =
  if check_lexeme syn { category = cat; child = word } then
    let node = Leaf { pair = create_lexeme cat word; id = newID () } in
    treelist := node :: !treelist
  else ()

let rec get node_id (lst : tree list) =
  match lst with
  | [] -> raise Wrong
  | h :: t -> (
      match h with
      | NodeOne { id } -> if node_id = id then h else get node_id t
      | NodeTwo { id } -> if node_id = id then h else get node_id t
      | Leaf { id } -> if node_id = id then h else get node_id t)

let remove removal_id (lst : tree list) =
  let rec internal prev lst =
    let rec claimed_ids tree =
      match tree with
      | NodeOne { t; id } -> id :: claimed_ids t
      | NodeTwo { t1; t2; id } -> id :: id :: (claimed_ids t1 @ claimed_ids t2)
      | Leaf { id } -> [ id ]
    in
    let () = print_endline "Hey" in
    match lst with
    | [] -> []
    | h :: t ->
        let id =
          match h with
          | NodeOne { id } -> id
          | NodeTwo { id } -> id
          | Leaf { id } -> id
        in
        if id = removal_id then
          let children =
            match h with
            | NodeOne { t } -> [ t ]
            | NodeTwo { t1; t2 } -> [ t1; t2 ]
            | Leaf _ -> []
          in
          let disallowed_ids = claimed_ids h in
          List.filter
            (fun node ->
              let id =
                match node with
                | NodeOne { id } -> id
                | NodeTwo { id } -> id
                | Leaf { id } -> id
              in
              not (List.mem id disallowed_ids))
            (List.rev prev @ children @ t)
        else internal (h :: prev) t
  in
  treelist := internal [] lst

let connect2 t1 t2 category syntax =
  let new_id = newID () in
  if
    check_constituent syntax
      (make_constituent_node (NodeTwo { t1; t2; category; id = new_id }))
  then
    let () =
      match t1 with
      | NodeOne { id } -> remove id !treelist
      | NodeTwo { id } -> remove id !treelist
      | Leaf { id } -> remove id !treelist
    in
    let () =
      match t2 with
      | NodeOne { id } -> remove id !treelist
      | NodeTwo { id } -> remove id !treelist
      | Leaf { id } -> remove id !treelist
    in
    treelist := NodeTwo { t1; t2; category; id = new_id } :: !treelist
  else raise Wrong

let connect1 tree const syn =
  let id = newID () in
  if
    check_constituent syn
      (make_constituent_node (NodeOne { t = tree; category = const; id }))
  then
    let () =
      match tree with
      | NodeOne { id } -> remove id !treelist
      | NodeTwo { id } -> remove id !treelist
      | Leaf { id } -> remove id !treelist
    in
    treelist := NodeOne { t = tree; category = const; id } :: !treelist
  else raise Wrong

let rec to_string tree =
  match tree with
  | Leaf { pair; id } ->
      "[" ^ string_of_lexeme pair ^ "] (id: " ^ string_of_int id ^ ")"
  | NodeOne { t; category; id } ->
      "[" ^ category ^ to_string t ^ "] (id: " ^ string_of_int id ^ ")"
  | NodeTwo { t1; t2; category; id } ->
      "[" ^ category ^ to_string t1 ^ to_string t2 ^ "] (id: "
      ^ string_of_int id ^ ")"
