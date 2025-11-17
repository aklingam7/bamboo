exception SyntaxParsingFailure

type syntax_doc = string

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

let rule_regexp_pattern = {|([^\b]*)\b->\b(([^\b]*)\|([^\b]*\b[^\b]*))|}
let rule_regexp = Str.regexp rule_regexp_pattern
let lhs_group_number, rhs_group_number = (1, 2)

let syntax_doc_regexp =
  let pattern =
    let category_regexp = Str.regexp "<cats>" in
    let rules_regexp = Str.regexp "<rules>" in
    Str.global_replace category_regexp
      ("\n" ^ {|(([^\b]*|} ^ "\n" ^ {|)*)|} ^ "\n")
      (Str.global_replace rules_regexp
         ("((" ^ rule_regexp_pattern ^ "\n)*)")
         {|lexical categories: <cats>constituent categories: <cats>rules: <rules>|})
  in
  Str.regexp pattern

let lexical_cats_group_number, constituent_cats_group_number = (1, 3)
let rules_group_number = 5
let whitespace = " "

let normalise_whitespace str =
  let whitespace_regexp = Str.regexp {|\b|} in
  Str.global_replace whitespace_regexp whitespace str

let entries categories_string = String.split_on_char '\n' categories_string

let build_syntax (sd : syntax_doc) : syntax =
  let parse_rule rule =
    let well_formed_rule = Str.string_match rule_regexp rule 0 in
    if not well_formed_rule then raise SyntaxParsingFailure;
    ( Str.matched_group lhs_group_number rule,
      normalise_whitespace (Str.matched_group rhs_group_number rule) )
  in
  let well_formed = Str.string_match syntax_doc_regexp sd 0 in
  if not well_formed then raise SyntaxParsingFailure;
  {
    lexical_categories =
      entries (Str.matched_group lexical_cats_group_number sd);
    constituent_categories =
      entries (Str.matched_group constituent_cats_group_number sd);
    ruleset =
      (let rules = entries (Str.matched_group rules_group_number sd) in
       List.map parse_rule rules);
  }

let check_lexeme (s : syntax) (l : lexical_node) : bool =
  List.mem l.category s.lexical_categories

let rec check_constituent (s : syntax) (c : constituent_node) : bool =
  let cat = c.category in
  let subs =
    let rules = List.filter (fun rule -> fst rule = cat) s.ruleset in
    List.map snd rules
  in
  match c.children with
  | L l -> List.mem l.category subs && check_lexeme s l
  | C c -> List.mem c.category subs && check_constituent s c
  | LC (l, c) ->
      List.mem (l.category ^ whitespace ^ c.category) subs
      && check_lexeme s l && check_constituent s c
  | CL (c, l) ->
      List.mem (c.category ^ whitespace ^ l.category) subs
      && check_constituent s c && check_lexeme s l
  | CC (c1, c2) ->
      List.mem (c1.category ^ whitespace ^ c2.category) subs
      && check_constituent s c1 && check_constituent s c2
