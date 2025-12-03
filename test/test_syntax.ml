open OUnit2
open Bamboo.Syntax

let sample_syntax =
  build_syntax
    "lexical categories:\n\
     N\n\
     V\n\
     D\n\
     constituent categories:\n\
     DP\n\
     VP\n\
     TP\n\
     NP\n\
     rules:\n\
     TP -> NP VP\n\
     NP -> N\n\
     DP -> D NP\n\
     VP -> V DP\n"

let invalid_syntax =
  "Noun\nconstituent categories:\nNP\nS\n\nrules:\nS -> NP VP\n"

let np = { category = "NP"; children = L { category = "N"; child = "Cayuga" } }

let dp_good =
  { category = "DP"; children = LC ({ category = "D"; child = "the" }, np) }

let dp_bad =
  { category = "DP"; children = LC ({ category = "Det"; child = "the" }, np) }

(* Note that this is not of type [test] but of type [test list]. This is so that
   it is easy to add new tests here but combine all related tests together later
   in the combined test suite *)
let syntax_tests =
  [
    ("a trivial test" >:: fun _ -> assert_equal 0 0 ~printer:string_of_int);
    ( "testing truthy check_lexeme" >:: fun _ ->
      assert (check_lexeme sample_syntax { category = "N"; child = "truth" }) );
    ( "testing falsey check_lexeme" >:: fun _ ->
      assert (
        not (check_lexeme sample_syntax { category = "Tense"; child = "false" }))
    );
    ( "testing faulty build_syntax" >:: fun _ ->
      assert_raises MalformedSyntax (fun () -> build_syntax invalid_syntax) );
    ( "testing truthy (L) check_constituent" >:: fun _ ->
      assert (check_constituent sample_syntax np) );
    ( "testing truthy (LC) check_constituent" >:: fun _ ->
      assert (check_constituent sample_syntax dp_good) );
    ( "testing falsey check_constituent" >:: fun _ ->
      assert (not (check_constituent sample_syntax dp_bad)) );
  ]
