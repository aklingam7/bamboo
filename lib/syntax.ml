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

let syntax_doc_regexp =
  let regexp =
    let newline_regex = Str.regexp "<n>" in
    Str.global_replace newline_regex "\n"
      {|lexical categories: <n>(.**)<n>constituent categories: <n>(.**)<n>rules: <n>(.**)<n>|}
  in
  Str.regexp regexp

let rule_regexp = Str.regexp {|(.*) -> (.*)|}
let entries categories_string = String.split_on_char '\n' categories_string

let build_syntax (sd : syntax_doc) : syntax =
  let parse_rule rule =
    let well_formed_rule = Str.string_match rule_regexp rule 0 in
    if not well_formed_rule then raise SyntaxParsingFailure;
    (Str.matched_group 1 rule, Str.matched_group 2 rule)
  in
  let well_formed = Str.string_match syntax_doc_regexp sd 0 in
  if not well_formed then raise SyntaxParsingFailure;
  {
    lexical_categories = entries (Str.matched_group 1 sd);
    constituent_categories = entries (Str.matched_group 2 sd);
    ruleset =
      (let rules = entries (Str.matched_group 3 sd) in
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
      List.mem (l.category ^ c.category) subs
      && check_lexeme s l && check_constituent s c
  | CL (c, l) ->
      List.mem (c.category ^ l.category) subs
      && check_constituent s c && check_lexeme s l
  | CC (c1, c2) ->
      List.mem (c1.category ^ c2.category) subs
      && check_constituent s c1 && check_constituent s c2
