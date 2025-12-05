open OUnit2
open Bamboo.Syntax

let expected_unmodified_syntax_string =
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

let sample_syntax = build_syntax expected_unmodified_syntax_string

let expected_modified_syntax_string =
  "lexical categories:\n\
   Adj\n\
   N\n\
   V\n\
   D\n\
   constituent categories:\n\
   AdjP\n\
   DP\n\
   VP\n\
   TP\n\
   NP\n\
   rules:\n\
   NP -> AdjP N\n\
   AdjP -> Adj\n\
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
  {
    category = "DP";
    children = CL (dp_good, { category = "N"; child = "cat" });
  }

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
    ( "testing check_constituent with CC (two constituent children)" >:: fun _ ->
      assert (check_constituent sample_syntax tp_good) );
    ( "testing check_constituent with CL" >:: fun _ ->
      (* This tests a constituent followed by a lexeme *)
      assert (not (check_constituent sample_syntax dp_with_lex));
      let preposition_syntax =
        build_syntax
          "lexical categories:\n\
           N\n\
           P\n\
           constituent categories:\n\
           NP\n\
           PP\n\
           rules:\n\
           NP -> N\n\
           PP -> NP P\n"
      in
      let p = { category = "P"; child = "with" } in
      let pp = { category = "PP"; children = CL (np, p) } in
      assert (check_constituent preposition_syntax pp) );
    ( "testing check_lexeme with V category" >:: fun _ ->
      assert (check_lexeme sample_syntax { category = "V"; child = "walks" }) );
    ( "testing check_lexeme with D category" >:: fun _ ->
      assert (check_lexeme sample_syntax { category = "D"; child = "a" }) );
    ( "testing check_constituent with VP" >:: fun _ ->
      assert (check_constituent sample_syntax vp) );
    ( "testing invalid constituent category" >:: fun _ ->
      let invalid =
        { category = "XP"; children = L { category = "N"; child = "cat" } }
      in
      assert (not (check_constituent sample_syntax invalid)) );
    ( "testing constituent with wrong child category for lexeme" >:: fun _ ->
      let wrong_lex =
        { category = "NP"; children = L { category = "V"; child = "run" } }
      in
      assert (not (check_constituent sample_syntax wrong_lex)) );
    ( "testing CC constituent with invalid rule" >:: fun _ ->
      let invalid_cc = { category = "NP"; children = CC (np, vp) } in
      assert (not (check_constituent sample_syntax invalid_cc)) );
    ( "testing constituent LC with wrong lexeme category" >:: fun _ ->
      let wrong_lc =
        {
          category = "DP";
          children = LC ({ category = "N"; child = "cat" }, np);
        }
      in
      assert (not (check_constituent sample_syntax wrong_lc)) );
    ( "testing valid C constituent under X-bar theory" >:: fun _ ->
      let x_bar_syntax =
        build_syntax
          "lexical categories:\n\
           N\n\
           constituent categories:\n\
           N'\n\
           NP\n\
           rules:\n\
           N' -> N\n\
           NP -> N'\n"
      in
      let valid_x_bar_c =
        {
          category = "NP";
          children =
            C
              {
                category = "N'";
                children = L { category = "N"; child = "cat" };
              };
        }
      in
      assert (check_constituent x_bar_syntax valid_x_bar_c) );
    ( "testing build_syntax with empty string" >:: fun _ ->
      assert_raises MalformedSyntax (fun () -> build_syntax "") );
    ( "testing build_syntax with missing sections" >:: fun _ ->
      let missing = "lexical categories:\nN\n" in
      assert_raises MalformedSyntax (fun () -> build_syntax missing) );
    ( "testing accuracy of to_string" >:: fun _ ->
      assert_equal expected_unmodified_syntax_string (to_string sample_syntax)
    );
    ( "testing all three categories for modify_syntax" >:: fun _ ->
      let test_file = Filename.temp_file "syntax" ".txt" in
      let () =
        Out_channel.with_open_text test_file (fun oc ->
            Out_channel.output_string oc expected_unmodified_syntax_string)
      in
      let step1_syntax =
        modify_syntax "lexical-categories" "Adj"
          (build_syntax expected_unmodified_syntax_string)
          (Some test_file)
      in
      let step2_syntax =
        modify_syntax "constituent-categories" "AdjP" step1_syntax
          (Some test_file)
      in
      let step3_syntax =
        modify_syntax "rules" "AdjP -> Adj" step2_syntax (Some test_file)
      in
      (* Result 1 used to verify file and syntax agreement if file is passed *)
      let result1_syntax =
        modify_syntax "rules" "NP -> AdjP N" step3_syntax (Some test_file)
      in
      let test_file_result1 =
        In_channel.with_open_text test_file (fun ic -> In_channel.input_all ic)
      in
      (* Result 2 used to verify that file and syntax can disagree if no file is
         passed *)
      let result2_syntax =
        modify_syntax "lexical-categories" "Potatoes" step3_syntax None
      in
      let test_file_result2 =
        In_channel.with_open_text test_file (fun ic -> In_channel.input_all ic)
      in
      assert_equal expected_modified_syntax_string (to_string result1_syntax)
        ~printer:(fun s -> s);
      assert_equal expected_modified_syntax_string test_file_result1
        ~printer:(fun s -> s);
      assert_bool
        "The modified syntax tree should not match the unmodified file. "
        (expected_modified_syntax_string <> to_string result2_syntax);
      assert_equal expected_modified_syntax_string test_file_result2
        ~printer:(fun s -> s);
      Sys.remove test_file );
    ( "testing duplicate lexical categories causes error" >:: fun _ ->
      assert_raises InvalidSyntax (fun () ->
          build_syntax
            "lexical categories:\nAdj\nAdj\n\nconstituent categories:\nrules:\n")
    );
    ( "testing duplicate constituent categories causes error" >:: fun _ ->
      assert_raises InvalidSyntax (fun () ->
          build_syntax
            "lexical categories:\nconstituent categories:\nAdjP\nAdjP\nrules:\n")
    );
    ( "testing malformed syntax causes error" >:: fun _ ->
      assert_raises MalformedSyntax (fun () ->
          build_syntax
            "lexical categories:\n\
             N\n\
             constituent categories:\n\
             NP\n\
             rules:\n\
             NP -> \n") );
    ( "testing duplicate rules causes error" >:: fun _ ->
      assert_raises InvalidSyntax (fun () ->
          build_syntax
            "lexical categories:\n\
             Adj\n\
             constituent categories:\n\
             AdjP\n\
             rules:\n\
             AdjP -> Adj\n\
             AdjP -> Adj\n") );
  ]
