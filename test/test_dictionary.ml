open OUnit2
open Bamboo.Dictionary

let raw_test_dict =
  [
    [ "Noun"; "knowledge" ];
    [ "Verb"; "learn" ];
    [ "Determiner"; "the" ];
    [ "Verb"; "inspire" ];
    [ "Noun"; "journey" ];
  ]

let invalid_dict_1 = [ [ "Noun" ] ]

let invalid_dict_2 =
  [
    [ "Noun"; "knowledge" ];
    [ "Verb"; "learn"; "extraneous element" ];
    [ "Determiner"; "the" ];
  ]

let invalid_dict_3 = [ [] ]

(* Note that this is not of type [test] but of type [test list]. This is so that
   it is easy to add new tests here but combine all related tests together later
   in the combined test suite *)
let dictionary_tests =
  [
    ("a trivial test" >:: fun _ -> assert_equal 0 0);
    (* Can't test creating a lexeme directly since it's an abstract type *)
    ( "testing creating a lexeme and stringifying it" >:: fun _ ->
      assert_equal
        (string_of_lexeme (create_lexeme "Noun" "cat"))
        "Category: Noun | Word: cat"
        ~printer:(fun id -> id) );
    (* Can't test building a dictionary directly since it's an abstract type *)
    ( "testing building a dictionary and checking for membership true"
    >:: fun _ ->
      assert (
        mem (create_lexeme "Determiner" "the") (build_dictionary raw_test_dict))
    );
    ( "testing building a dictionary and checking for membership false"
    >:: fun _ ->
      assert (
        not
          (mem
             (create_lexeme "Noun" "failure")
             (build_dictionary raw_test_dict))) );
    ( "testing building an invalid dictionary and asserting failure 1"
    >:: fun _ ->
      assert_raises
        (Failure
           "The dictionary received is of an invalid type. The CSV must be of \
            size nx2") (fun () -> build_dictionary invalid_dict_1) );
    ( "testing building an invalid dictionary and asserting failure 2"
    >:: fun _ ->
      assert_raises
        (Failure
           "The dictionary received is of an invalid type. The CSV must be of \
            size nx2") (fun () -> build_dictionary invalid_dict_2) );
    ( "testing building an invalid dictionary and asserting failure 3"
    >:: fun _ ->
      assert_raises
        (Failure
           "The dictionary received is of an invalid type. The CSV must be of \
            size nx2") (fun () -> build_dictionary invalid_dict_3) );
    (* Can't test building a suppressed dictionary directly since it's an
       abstract type *)
    ( "testing building a suppressed dictionary and asserting everything is a \
       member 1"
    >:: fun _ ->
      assert (mem (create_lexeme "Determiner" "the") suppressed_dictionary) );
    ( "testing building a suppressed dictionary and asserting everything is a \
       member 2"
    >:: fun _ -> assert (mem (create_lexeme "ç̷̧̯̬̱̠̗͓̝̟̃́̊̏̓͑́̚͘ã̸̧̛̪̠͇̠̥̳̎̀́̀̄̈́̃̉̒̂͊͝t̸̨͔̯͉̣̟͈̼͖̻̮̥̱̀͒̄͜" "w̸̟̮̪̘̮̫̬̳̪͍̭͓̹̤̾̿͝ͅo̸̧̡̱̲̣̫̔̈́͌͝r̸̛̪̱̳̣̩̼̊̾̀͆̅̏̑͑̔͗̆̕̕͜ḓ̷̇̀̈́̋̄̉̀̕ͅ") suppressed_dictionary)
    );
    ( "testing building a suppressed dictionary and asserting everything is a \
       member 3"
    >:: fun _ ->
      assert (
        mem
          (create_lexeme "Noun" "totally_not_a_real_word")
          suppressed_dictionary) );
    ( "testing dictionary length" >:: fun _ ->
      assert (len_dictionary (build_dictionary raw_test_dict) = 5) );
    ( "testing suppressed dictionary length" >:: fun _ ->
      assert (len_dictionary suppressed_dictionary = -1) );
    ( "testing augmenting a dictionary" >:: fun _ ->
      assert (
        len_dictionary (build_dictionary raw_test_dict) + 1
        = len_dictionary
            (augment_dictionary
               (create_lexeme "Determiner" "the")
               (build_dictionary raw_test_dict))) );
  ]
