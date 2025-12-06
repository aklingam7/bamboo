exception InvalidSyntax
exception MalformedSyntax
exception NonexistentRuleType

(** Abstraction function: The [lexical_categories] list [l1; ...; ln] represents
    the n distinct lexical categories recognized as valid by the syntax, using
    their names or shorthand. The [constituent_categories] list [c1; ... ;cn]
    similarly represents the n distinct constituent categories recognized by the
    syntax. The [rulset] [c1 * s1; ...; c1 * sn] list represents the acceptable
    constituent structures of the syntax, with each category coming first in the
    pair, followed by the categories of the constituents it is composed of.
    Representation invariant:
    - [lexical_categories] contains no duplicate entries
    - [constituent_categories] contains no duplicate entries
    - [ruleset] may contain arbitrary category names (not necessarily present in
      the category lists), as the interface specifies that extraneous categories
      are accepted on purpose
    - [ruleset] may contain duplicate rules *)

type syntax = {
  lexical_categories : string list;
  constituent_categories : string list;
  ruleset : (string * string) list;
}

type lexical_node = {
  category : string;
  child : string;
}

type constituent_children =
  | CC of (constituent_node * constituent_node)
  | CL of (constituent_node * lexical_node)
  | LC of (lexical_node * constituent_node)
  | C of constituent_node
  | L of lexical_node

and constituent_node = {
  category : string;
  children : constituent_children;
}

(** Raises [InvalidSyntax] if invalid. *)
let validate_syntax (s : syntax) : unit =
  let duplicates lst =
    List.length (List.sort_uniq compare lst) <> List.length lst
  in
  if
    duplicates s.lexical_categories
    || duplicates s.constituent_categories
    || duplicates s.ruleset
  then raise InvalidSyntax

let rule_regexp_pattern = {|\([^ ]+\) -> \([^ ]+\( [^ ]+\)?\)|}
let rule_regexp = Str.regexp rule_regexp_pattern
let lhs_group_number, rhs_group_number = (1, 2)

let replace_newline_escape str =
  let newline_escape_regexp = Str.regexp {|<n>|} in
  Str.global_replace newline_escape_regexp "\n" str

let syntax_doc_regexp =
  let header1 = "lexical categories:" in
  let header2 = "constituent categories:" in
  let header3 = "rules:" in
  let cats = replace_newline_escape {|\(\([^ ]+<n>\)*\)|} in
  let rules =
    replace_newline_escape ({|\(\(|} ^ rule_regexp_pattern ^ {|<n>\)*\)|})
  in
  Str.regexp
    [%string "%{header1}\n%{cats}%{header2}\n%{cats}%{header3}\n%{rules}"]

let lexical_cats_group_number, constituent_cats_group_number = (1, 3)
let rules_group_number = 5

(** [entries str] converts newline-terminated entries to [string list]. Requires
    there is at least one entry in [str]. *)
let entries str =
  let rec remove_last = function
    | [ _ ] -> []
    | h :: t -> h :: remove_last t
    | _ -> ( [] [@coverage off])
    (* The previous two cases block all cases which obey the precondition, so
       this line being reached requires a precondition violation. Thus it is
       ignored. *)
  in
  remove_last (String.split_on_char '\n' str)

(** Returns [true] on exact (complete) match and updates [Str.matched_string s]
    if true *)
let full_match r s =
  Str.string_match r s 0
  && String.length (Str.matched_string s) = String.length s

let build_syntax (sd : string) : syntax =
  (* Helper to parse rules *)
  let parse_rule rule =
    let _ = full_match rule_regexp rule in
    ( Str.matched_group lhs_group_number rule,
      Str.matched_group rhs_group_number rule )
  in

  (* Parse sections *)
  let well_formed = full_match syntax_doc_regexp sd in
  if not well_formed then raise MalformedSyntax;

  (* Construct syntax *)
  let lexical_categories =
    entries (Str.matched_group lexical_cats_group_number sd)
  in
  let constituent_categories =
    entries (Str.matched_group constituent_cats_group_number sd)
  in
  let ruleset =
    let rules = entries (Str.matched_group rules_group_number sd) in
    List.map parse_rule rules
  in
  let syntax = { lexical_categories; constituent_categories; ruleset } in

  validate_syntax syntax;
  syntax

let check_lexeme (s : syntax) (l : lexical_node) : bool =
  List.mem l.category s.lexical_categories

let rec check_constituent (s : syntax) (c : constituent_node) : bool =
  let cat = c.category in
  let subs =
    let rules = List.filter (fun rule -> fst rule = cat) s.ruleset in
    List.map snd rules
  in
  List.mem c.category s.constituent_categories
  &&
  match c.children with
  | L l -> List.mem l.category subs && check_lexeme s l
  | C c -> List.mem c.category subs && check_constituent s c
  | LC (l, c) ->
      List.mem (l.category ^ " " ^ c.category) subs
      && check_lexeme s l && check_constituent s c
  | CL (c, l) ->
      List.mem (c.category ^ " " ^ l.category) subs
      && check_constituent s c && check_lexeme s l
  | CC (c1, c2) ->
      List.mem (c1.category ^ " " ^ c2.category) subs
      && check_constituent s c1 && check_constituent s c2

let to_string (s : syntax) =
  "lexical categories:\n"
  ^ String.concat "\n" s.lexical_categories
  ^ "\nconstituent categories:\n"
  ^ String.concat "\n" s.constituent_categories
  ^ "\nrules:\n"
  ^ String.concat "\n" (List.map (fun r -> fst r ^ " -> " ^ snd r) s.ruleset)
  ^ "\n"

let modify_syntax category rule syntax file_opt =
  (* Reading the current syntax and turning each line into a list element *)
  let syntax_lines = String.split_on_char '\n' (to_string syntax) in

  (* The rule for where to add the new syntax item *)
  let match_category line =
    if
      (line = "lexical categories:" && category = "lexical-categories")
      || line = "constituent categories:"
         && category = "constituent-categories"
      || (line = "rules:" && category = "rules")
    then true
    else false
  in

  (* Inserting into the syntax line list *)
  let rec insert_rule = function
    | line :: file_t when match_category line -> line :: rule :: file_t
    | line :: file_t -> line :: insert_rule file_t
    | _ -> raise NonexistentRuleType
  in

  (* Evaluating to find the new syntax rules *)
  let new_syntax = insert_rule syntax_lines in
  let new_syntax_text = String.concat "\n" new_syntax in

  (* Modifying everything that needs to be modified *)
  let syntax_text = new_syntax_text in
  let new_syntax =
    (* We need to rebuild syntax everytime to ensure that the new syntax is well
       formed, which is known to be checked by build_syntax*)
    build_syntax syntax_text
  in
  let () =
    match file_opt with
    | None -> ()
    | Some path ->
        Out_channel.with_open_text path (fun oc ->
            Out_channel.output_string oc new_syntax_text)
  in
  new_syntax
