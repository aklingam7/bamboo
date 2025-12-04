type lexeme
(** [lexeme] represents a word paired with its lexical category (e.g., noun,
    verb). This is an abstract type to ensure lexemes are only created via the
    proper constructor function. *)

type dictionary
(** [dictionary] represents a collection of valid lexemes that can be used in
    syntax tree construction. The dictionary serves as a membership set for
    validating word-category pairs. *)

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

val augment_dictionary : lexeme -> dictionary -> dictionary
(** [augment_dictionary lexeme dictionary] adds the passed in lexeme to the
    dictionary being used. *)

val mem : lexeme -> dictionary -> bool
(** [mem lexeme dictionary] checks if the given lexeme is in the provided
    dictionary. *)

val len_dictionary : dictionary -> int
(** [len_dictionary dictionary] is the length of the dictionary, i.e. how many
    lexeme entries can be found in the dictionary. This is a utility function
    that can be used where ever needed, such as in test cases and dictionary
    validation. *)

(**val lookup_dictionary : string -> dictionary -> string list (**
   [lookup_dictionary word dict] returns the list of lexical categories
   associated with word [word]. Requires [word] is a string and [dict] is a
   dictionary.*)

   val has_word : string -> dictionary -> bool (** [has_word word dict] returns
   true if [word] is found in [dict] and false otherwise. Requires [word] is a
   string and [dict] is a dictionary*)*)
