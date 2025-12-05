open Tree
open Dictionary

let rec to_string (tree_workspace : tree) : string =
  let rec tree_to_string (tree : tree_node) : string =
    match tree with
    | Leaf { pair; id } ->
        "[" ^ string_of_lexeme pair ^ "] (" ^ string_of_int id ^ ")"
    | NodeOne { t; category; id } ->
        "[" ^ category ^ tree_to_string t ^ "] (" ^ string_of_int id ^ ")"
    | NodeTwo { t1; t2; category; id } ->
        "[" ^ category ^ tree_to_string t1 ^ tree_to_string t2 ^ "] ("
        ^ string_of_int id ^ ")"
  in
  let nodes = to_list tree_workspace in
  match nodes with
  | [] -> ""
  | node :: rest ->
      List.fold_left (fun acc node -> acc ^ tree_to_string node ^ "\n") "" nodes

type fancy_string = {
  rep : string;
  mid : int;
  width : int;
}
(** Represents a formatted string with positioning information for tree
    rendering. Used internally by fancy printing functions to calculate spacing
    and alignment.
    - [rep]: The string representation with newlines and spacing
    - [mid]: The midpoint position for vertical line alignment
    - [width]: The total width of the string in characters *)

let one_padding sub cur =
  let dif = cur - sub in
  let l_pad = abs dif / 2 in
  let r_pad = if abs dif mod 2 = 1 then l_pad + 1 else l_pad in
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

let rec to_fancy_string_helper (node : tree_node) : fancy_string =
  match node with
  | Leaf { pair; id } ->
      let top = get_lexeme_category pair ^ " (" ^ string_of_int id ^ ")" in
      let bottom = get_lexeme_word pair in
      let padding = one_padding (String.length bottom) (String.length top) in
      let top = add_single_padding top (fst padding) in
      let bottom = add_single_padding bottom (snd padding) in
      let width = String.length top in
      let mid = (width - 1) / 2 in
      let rep =
        top ^ "\n"
        ^ (let row = String.make width ' ' in
           String.mapi (fun i _ -> if i = mid then '|' else ' ') row)
        ^ "\n" ^ bottom
      in
      { rep; mid; width }
  | NodeOne { t; category; id } ->
      let sub = to_fancy_string_helper t in
      let cur = "[" ^ category ^ "] (" ^ string_of_int id ^ ")" in
      let padding = one_padding sub.width (String.length cur) in
      let sub = { sub with rep = add_multiple_padding sub.rep (snd padding) } in
      let cur = add_single_padding cur (fst padding) in
      let width = String.length cur in
      let mid = (width - 1) / 2 in
      let temp_rep = { rep = cur; mid; width } in
      { rep = one_branch sub temp_rep; mid; width }
  | NodeTwo { t1; t2; category; id } ->
      let sub1 = to_fancy_string_helper t1 in
      let sub2 = to_fancy_string_helper t2 in
      let cur = "[" ^ category ^ "] (" ^ string_of_int id ^ ")" in
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
      let mid = (width - 1) / 2 in
      let temp_rep = { rep = cur; mid; width } in
      { rep = two_branch sub1 sub2 (fst subs) temp_rep; mid; width }

let rec to_fancy_string (t : tree) : string =
  match to_list t with
  | [] -> ""
  | node :: rest ->
      let start = to_fancy_string_helper node in
      fst
        (List.fold_left
           (fun (acc_rep, acc_width) n ->
             let next = to_fancy_string_helper n in
             let combined_rep, combined_width =
               combine_subs { rep = acc_rep; mid = 0; width = acc_width } next
             in
             (combined_rep, combined_width))
           (start.rep, start.width) rest)
