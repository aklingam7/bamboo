open Syntax
open Dictionary

exception Wrong

(**Abstraction function: [Leaf {pair = (a,b); id = x}] represents a node
   containing a lexical category [a] and a word of its type [b], with an
   associated id number [x]. [NodeOne {t = tr; category = n; id = x}] represents
   a recursive tree containing a constituent node of category [n], which
   branches down into subtree [tr]. The tree has an associated id number [x].
   [NodeTwo {t1 = tr1; t2 = tr2; category = n; id = x}] represents a recursive
   tree containing a constituent branching node of category [n], which branches
   down into subtree [tr1] and [tr2]. The tree has an associated id number [x].
   Representation Invariants: [id] >= 0; both [t1] and [t2] in [NodeTwo] can not
   have their respective [category] define lexical nodes.*)
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

let add cat word syn dict trlst =
  if
    check_lexeme syn { category = cat; child = word }
    && mem (create_lexeme cat word) dict
  then
    let node = Leaf { pair = create_lexeme cat word; id = newID () } in
    trlst := !trlst @ [ node ]
  else raise Wrong

let rec get node_id (lst : tree list) =
  match lst with
  | [] -> raise Wrong
  | h :: t -> (
      match h with
      | NodeOne { id } -> if node_id = id then h else get node_id t
      | NodeTwo { id } -> if node_id = id then h else get node_id t
      | Leaf { id } -> if node_id = id then h else get node_id t)

let remove removal_id (lst : tree list ref) =
  let rec internal prev lst =
    let rec claimed_ids tree =
      match tree with
      | NodeOne { t; id } -> id :: claimed_ids t
      | NodeTwo { t1; t2; id } -> id :: id :: (claimed_ids t1 @ claimed_ids t2)
      | Leaf { id } -> [ id ]
    in
    let start_claim tree =
      match tree with
      | NodeOne { t } -> claimed_ids t
      | NodeTwo { t1; t2 } -> claimed_ids t1 @ claimed_ids t2
      | Leaf _ -> []
    in
    match lst with
    | [] -> raise Wrong
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
          let disallowed_ids = List.flatten (List.map start_claim children) in
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
  lst := internal [] !lst

let fade fade_id (lst : tree list ref) =
  let rec internal prev lst =
    match lst with
    | [] -> []
    | h :: t ->
        let id =
          match h with
          | NodeOne { id } -> id
          | NodeTwo { id } -> id
          | Leaf { id } -> id
        in
        if id = fade_id then List.rev prev @ t else internal (h :: prev) t
  in
  lst := internal [] !lst

let connect2 t1 t2 category syntax trlst =
  let new_id = newID () in
  if
    check_constituent syntax
      (make_constituent_node (NodeTwo { t1; t2; category; id = new_id }))
  then
    let () =
      match t1 with
      | NodeOne { id } -> fade id trlst
      | NodeTwo { id } -> fade id trlst
      | Leaf { id } -> fade id trlst
    in
    let () =
      match t2 with
      | NodeOne { id } -> fade id trlst
      | NodeTwo { id } -> fade id trlst
      | Leaf { id } -> fade id trlst
    in
    trlst := NodeTwo { t1; t2; category; id = new_id } :: !trlst
  else raise Wrong

let connect1 tree const syn trlst =
  let id = newID () in
  if
    check_constituent syn
      (make_constituent_node (NodeOne { t = tree; category = const; id }))
  then
    let () =
      match tree with
      | NodeOne { id } -> fade id trlst
      | NodeTwo { id } -> fade id trlst
      | Leaf { id } -> fade id trlst
    in
    trlst := NodeOne { t = tree; category = const; id } :: !trlst
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

(** Represents a formatted string with positioning information for tree rendering.
    Used internally by fancy printing functions to calculate spacing and alignment.
    - [rep]: The string representation with newlines and spacing
    - [mid]: The midpoint position for vertical line alignment
    - [width]: The total width of the string in characters *)
type fancy_string = {
  rep : string;
  mid : int;
  width : int;
}

let one_padding sub cur =
  let dif = cur - sub in
  let l_pad = abs dif / 2 in
  let r_pad = if dif mod 2 = 1 then l_pad + 1 else l_pad in
  if dif > 0 then ((0, 0), (l_pad, r_pad)) else ((l_pad, r_pad), (0, 0))

let add_single_padding cur (l_pad, r_pad) =
  String.make l_pad ' ' ^ cur ^ String.make r_pad ' '

let add_multiple_padding rep (l_pad, r_pad) =
  let rep_list = String.split_on_char '\n' rep in
  let new_list =
    List.map (fun x -> add_single_padding x (l_pad, r_pad)) rep_list
  in
  String.concat "\n" new_list

let combine_subs sub1 sub2 =
  let sub1_list = String.split_on_char '\n' sub1.rep in
  let sub1_height = List.length sub1_list in
  let sub2_list = String.split_on_char '\n' sub2.rep in
  let sub2_height = List.length sub2_list in
  let dif = abs (sub1_height - sub2_height) in
  let sub1_list =
    if sub1_height < sub2_height then
      sub1_list @ List.init dif (fun _ -> String.make sub1.width ' ')
    else sub1_list
  in
  let sub2_list =
    if sub2_height < sub1_height then
      sub2_list @ List.init dif (fun _ -> String.make sub2.width ' ')
    else sub2_list
  in
  let subs_list =
    List.map2 (fun row1 row2 -> row1 ^ " " ^ row2) sub1_list sub2_list
  in
  let width = String.length (List.hd subs_list) in
  (String.concat "\n" subs_list, width)

let one_branch sub cur =
  let pos = cur.mid in
  let row = String.make cur.width ' ' in
  let row = String.mapi (fun i _ -> if i = pos then '|' else ' ') row in
  cur.rep ^ "\n" ^ row ^ "\n" ^ sub.rep

let two_branch sub1 sub2 subs cur =
  let dif1 = cur.mid - sub1.mid in
  let dif2 = cur.mid - sub2.mid in
  let pos1 = ref (cur.mid - 1) in
  let pos2 = ref (cur.mid + 1) in
  let acc = ref "" in
  let l_reached = ref false in
  let r_reached = ref false in
  let l_symbol = ref '/' in
  let r_symbol = ref '\\' in
  let () =
    for i = 1 to max (abs dif1) (abs dif2) / 2 do
      let row = String.make cur.width ' ' in
      let row =
        String.mapi
          (fun j _ ->
            if j = !pos1 then !l_symbol
            else if j = !pos2 then !r_symbol
            else ' ')
          row
      in
      let () = acc := !acc ^ row ^ "\n" in
      let () =
        if not !l_reached then
          if !pos1 > sub1.mid then pos1 := !pos1 - 2
          else
            let () = l_reached := true in
            l_symbol := '|'
      in
      let () =
        if not !r_reached then
          if !pos2 < sub2.mid + sub1.width then pos2 := !pos2 + 2
          else
            let () = r_reached := true in
            r_symbol := '|'
      in
      ()
    done
  in
  cur.rep ^ "\n" ^ !acc ^ subs

let rec to_fancy_string_helper (tree : tree) : fancy_string =
  match tree with
  | Leaf { pair; id } ->
      let top = get_lexeme_category pair ^ " (" ^ string_of_int id ^ ")" in
      let bottom = get_lexeme_word pair in
      let padding = one_padding (String.length bottom) (String.length top) in
      let top = add_single_padding top (fst padding) in
      let bottom = add_single_padding bottom (snd padding) in
      let width = String.length top in
      let mid = width / 2 in
      let rep =
        top ^ "\n"
        ^ (let row = String.make width ' ' in
           String.mapi (fun i _ -> if i = mid then '|' else ' ') row)
        ^ "\n" ^ bottom
      in
      { rep; mid; width }
  | NodeOne { t; category; id } ->
      let sub = to_fancy_string_helper t in
      let cur = "[" ^ category ^ "] (id: " ^ string_of_int id ^ ")" in
      let padding = one_padding sub.width (String.length cur) in
      let sub = { sub with rep = add_multiple_padding sub.rep (snd padding) } in
      let cur = add_single_padding cur (fst padding) in
      let width = String.length cur in
      let mid = width / 2 in
      let temp_rep = { rep = cur; mid; width } in
      { rep = one_branch sub temp_rep; mid; width }
  | NodeTwo { t1; t2; category; id } ->
      let sub1 = to_fancy_string_helper t1 in
      let sub2 = to_fancy_string_helper t2 in
      let cur = "[" ^ category ^ "] (id: " ^ string_of_int id ^ ")" in
      let subs = combine_subs sub1 sub2 in
      let padding = one_padding (snd subs) (String.length cur) in
      let sub1 =
        { sub1 with rep = add_multiple_padding sub1.rep (snd padding) }
      in
      let sub2 =
        { sub2 with rep = add_multiple_padding sub2.rep (snd padding) }
      in
      let cur = add_single_padding cur (fst padding) in
      let width = String.length cur in
      let mid = width / 2 in
      let temp_rep = { rep = cur; mid; width } in
      { rep = two_branch sub1 sub2 (fst subs) temp_rep; mid; width }

let to_fancy_string (tree : tree) : string = (to_fancy_string_helper tree).rep

let to_fancy_string_list (trees : tree list) : string =
  match trees with
  | [] -> ""
  | t :: ts ->
      let start = to_fancy_string_helper t in
      fst
        (List.fold_left
           (fun (acc_rep, acc_width) tree ->
             let next = to_fancy_string_helper tree in
             let combined_rep, combined_width =
               combine_subs { rep = acc_rep; mid = 0; width = acc_width } next
             in
             (combined_rep, combined_width))
           (start.rep, start.width) ts)
