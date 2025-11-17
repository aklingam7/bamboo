type lexeme
type dictionary

val create_lexeme : string -> string -> lexeme
(** [create_lexeme lexical_category word] creates a new lexeme from the provided
    inputs for futher use in other functions. *)

val string_of_lexeme : lexeme -> string
(** [string_of_lexeme lexeme] converts a lexeme to a human readable string
    representation. *)

val get_lexeme_category : lexeme -> string
(** [get_lexeme_category lexeme] returns the category of the lexeme passed in*)

val get_lexeme_word : lexeme -> string
(** [get_lexeme_word lexeme] returns the word for the lexeme passed in*)

val build_dictionary : string list list -> dictionary
(** [build_dictionary data] takes in a table and constructs a dictionary. [data]
    is a nx2 string list of lists associating a lexical category (noun,
    determiner, verb, etc.) with a word in the given language. Raises a
    [Failure] if data is not of size nx2. *)

val mem : lexeme -> dictionary -> bool
(** [mem lexeme dictionary] checks if the given lexeme is in the provided
    dictionary. *)
