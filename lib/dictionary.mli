type dictionary

val build_dictionary : string list list -> dictionary
(** [build_dictionary raw_data] takes in an already read dictionary CSV and
    constructs a dictionary. [raw_data] is a string list of lists associating a
    lexical category (noun, determiner, verb, etc.) with a word in the given
    language *)

val mem : string -> dictionary -> bool
(** [mem word dictionary] checks if the given word is in the provided dictionary
*)
