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

let vp =
  {
    category = "VP";
    children = LC ({ category = "V"; child = "runs" }, dp_good);
  }

let tp_good = { category = "TP"; children = CC (np, vp) }

let dp_with_lex =
  { category = "DP"; children = CL (dp_good, { category = "N"; child = "cat" }) }

(* Note that this is not of type [test] but of type [test list]. This is so that
   it is easy to add new tests here but combine all related tests together later
   in the combined test suite *)
let syntax_tests =
  [
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
    ( "testing check_constituent with C (single constituent child)" >:: fun _ ->
      let c_np = { category = "NP"; children = C np } in
      (* This should fail as NP -> NP is not a valid rule *)
      assert (not (check_constituent sample_syntax c_np)) );
    ( "testing check_constituent with CC (two constituent children)"
    >:: fun _ -> assert (check_constituent sample_syntax tp_good) );
    ( "testing check_constituent with CL" >:: fun _ ->
      (* This tests a constituent followed by a lexeme *)
      assert (not (check_constituent sample_syntax dp_with_lex)) );
    ( "testing check_lexeme with V category" >:: fun _ ->
      assert (check_lexeme sample_syntax { category = "V"; child = "walks" }) );
    ( "testing check_lexeme with D category" >:: fun _ ->
      assert (check_lexeme sample_syntax { category = "D"; child = "a" }) );
    ( "testing check_constituent with VP" >:: fun _ ->
      assert (check_constituent sample_syntax vp) );
    ( "testing invalid constituent category" >:: fun _ ->
      let invalid = { category = "XP"; children = L { category = "N"; child = "cat" } } in
      assert (not (check_constituent sample_syntax invalid)) );
    ( "testing constituent with wrong child category for lexeme" >:: fun _ ->
      let wrong_lex = { category = "NP"; children = L { category = "V"; child = "run" } } in
      assert (not (check_constituent sample_syntax wrong_lex)) );
    ( "testing CC constituent with invalid rule" >:: fun _ ->
      let invalid_cc = { category = "NP"; children = CC (np, vp) } in
      assert (not (check_constituent sample_syntax invalid_cc)) );
    ( "testing constituent LC with wrong lexeme category" >:: fun _ ->
      let wrong_lc =
        { category = "DP"; children = LC ({ category = "N"; child = "cat" }, np) }
      in
      assert (not (check_constituent sample_syntax wrong_lc)) );
    ( "testing build_syntax with empty string" >:: fun _ ->
      assert_raises MalformedSyntax (fun () -> build_syntax "") );
    ( "testing build_syntax with missing sections" >:: fun _ ->
      let missing = "lexical categories:\nN\n" in
      assert_raises MalformedSyntax (fun () -> build_syntax missing) );
  ]
