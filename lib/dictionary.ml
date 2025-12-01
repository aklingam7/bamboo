(**

Abstraction functions: The tuple (category, word) represents the abstract
concept of a lexeme of the form {Word_Category, Word}.
Representation invariant: This cannot be empty.

Abstraction functions: The list [l1; ...; ln] represents the dictionary
[{Word_Category_1, Word_1}, ..., {Word_Category_n, Word_n}].
The empty list [] represents an empty dictionary.
Representation invariant: Each entry in the dictionary must statisfy the
lexeme invariant.
*)

type lexeme = string * string

type dictionary =
  | Suppressed
  | Dictionary of lexeme list

let create_lexeme lexical_category word : lexeme = (lexical_category, word)

let string_of_lexeme ((cat, word) : lexeme) =
  "Category: " ^ cat ^ " | Word: " ^ word

let get_lexeme_category ((cat, _) : lexeme) = cat
let get_lexeme_word ((_, word) : lexeme) = word

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
  Dictionary (builder data)

let suppressed_dictionary = Suppressed

let augment_dictionary lex (dict : dictionary) =
  match dict with
  | Suppressed -> Suppressed
  | Dictionary old -> Dictionary (lex :: old)

let mem lex (dict : dictionary) : bool =
  let rec checker (lex : lexeme) dict =
    match dict with
    | [] -> false
    | lx :: dict_tl -> if lex = lx then true else checker lex dict_tl
  in

  match dict with
  | Suppressed ->
      true
      (* Because suppressed is used when we want to permit every possible word,
         i.e. no dictionary membership checking *)
  | Dictionary list -> checker lex list
