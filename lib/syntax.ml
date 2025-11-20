exception InvalidSyntax
exception MalformedSyntax

(** Abstraction function: The [lexical_categories] list [l1; ...; ln] represents
    the n distinct lexical categories recognized as valid by the syntax, using
    their names or shorthand. The [constituent_categories] list [c1; ... ;cn]
    similarly represents the n distinct constituent categories recognized by the
    syntax. The [rulset] [c1 * s1; ...; c1 * sn] list represents the acceptable
    constituent structures of the syntax, with each category coming first in the
    pair, followed by the categories of the constituents it is composed of.
    Representation invariant: The lists are internally consistent; i.e., there
    is no category in the rules of ruleset which is not contained within
    lexical_categories or constituent_categories. *)

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
  if duplicates s.lexical_categories || duplicates s.constituent_categories then
    raise InvalidSyntax

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

(** Converts newline-terminated entries to [string list] *)
let entries str =
  let rec remove_last = function
    | [] -> []
    | [ _ ] -> []
    | h :: t -> h :: remove_last t
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
    let well_formed_rule = full_match rule_regexp rule in
    if not well_formed_rule then raise MalformedSyntax;
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
