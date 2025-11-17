type lexeme = string * string
type dictionary = lexeme list

let create_lexeme lexical_category word : lexeme = (lexical_category, word)

let string_of_lexeme ((cat, word) : lexeme) =
  "Category: " ^ cat ^ " | Word: " ^ word

let get_lexeme_category (cat, _) = cat
let get_lexeme_word (_, word) = word

let build_dictionary data : dictionary =
  let rec builder d =
    match d with
    | [] -> []
    | [ cat; word ] :: tl -> create_lexeme cat word :: builder tl
    | _ ->
        failwith
          "The dictionary received is of an invalid type. The CSV must be of \
           size nx2"
  in
  builder data

let mem lex (dict : dictionary) : bool =
  let rec checker (lex : lexeme) dict =
    match dict with
    | [] -> false
    | lx :: dict_tl -> if lex = lx then true else checker lex dict_tl
  in
  checker lex dict
