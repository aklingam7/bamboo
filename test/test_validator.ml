open OUnit2
open Bamboo.Validator
open Bamboo.Dictionary
open Bamboo.Syntax
open Bamboo.Tree

let sample_dict =
  build_dictionary [ [ "N"; "cat" ]; [ "V"; "runs" ]; [ "D"; "the" ] ]

let sample_syntax =
  build_syntax
    "lexical categories:\n\
     N\n\
     V\n\
     D\n\
     constituent categories:\n\
     NP\n\
     VP\n\
     DP\n\
     rules:\n\
     NP -> N\n\
     VP -> V\n\
     VP -> V DP\n\
     DP -> D NP\n"

let validator_tests =
  [
    ( "validate_lexeme with Some dict - valid word" >:: fun _ ->
      assert (validate_lexeme "N" "cat" sample_syntax (Some sample_dict)) );
    ( "validate_lexeme with Some dict - invalid word" >:: fun _ ->
      assert (
        not (validate_lexeme "N" "dog" sample_syntax (Some sample_dict))) );
    ( "validate_lexeme with Some dict - invalid category" >:: fun _ ->
      assert (
        not (validate_lexeme "P" "cat" sample_syntax (Some sample_dict))) );
    ( "validate_lexeme with None - valid category" >:: fun _ ->
      assert (validate_lexeme "N" "anyword" sample_syntax None) );
    ( "validate_lexeme with None - invalid category" >:: fun _ ->
      assert (not (validate_lexeme "P" "anyword" sample_syntax None)) );
    ( "validate_lexeme with None - any valid word passes" >:: fun _ ->
      assert (validate_lexeme "V" "walks" sample_syntax None) );
    ( "validate_constituent with Leaf node" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      assert (validate_constituent leaf "NP" sample_syntax) );
    ( "validate_constituent with invalid Leaf" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      assert (not (validate_constituent leaf "VP" sample_syntax)) );
    ( "validate_constituent with NodeOne - valid" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let node1 = NodeOne { t = leaf; category = "NP"; id = 2 } in
      assert (validate_constituent node1 "NP" sample_syntax) );
    ( "validate_constituent with NodeOne - invalid" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "V" "runs"; id = 1 } in
      let node1 = NodeOne { t = leaf; category = "VP"; id = 2 } in
      assert (not (validate_constituent node1 "NP" sample_syntax)) );
    ( "validate_constituent with NodeTwo - both leaves" >:: fun _ ->
      let leaf1 = Leaf { pair = create_lexeme "D" "the"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "N" "cat"; id = 2 } in
      let node_np = NodeOne { t = leaf2; category = "NP"; id = 3 } in
      let node2 = NodeTwo { t1 = leaf1; t2 = node_np; category = "DP"; id = 4 } in
      assert (validate_constituent node2 "DP" sample_syntax) );
    ( "validate_constituent with NodeTwo - invalid rule" >:: fun _ ->
      let leaf1 = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "V" "runs"; id = 2 } in
      let node_np = NodeOne { t = leaf1; category = "NP"; id = 3 } in
      let node_vp = NodeOne { t = leaf2; category = "VP"; id = 4 } in
      let node2 = NodeTwo { t1 = node_np; t2 = node_vp; category = "DP"; id = 5 } in
      assert (not (validate_constituent node2 "DP" sample_syntax)) );
    ( "validate_constituent with complex nested structure" >:: fun _ ->
      let leaf_v = Leaf { pair = create_lexeme "V" "runs"; id = 1 } in
      let leaf_d = Leaf { pair = create_lexeme "D" "the"; id = 2 } in
      let leaf_n = Leaf { pair = create_lexeme "N" "cat"; id = 3 } in
      let node_np = NodeOne { t = leaf_n; category = "NP"; id = 4 } in
      let node_dp = NodeTwo { t1 = leaf_d; t2 = node_np; category = "DP"; id = 5 } in
      let node_vp =
        NodeTwo { t1 = leaf_v; t2 = node_dp; category = "VP"; id = 6 }
      in
      assert (validate_constituent node_vp "VP" sample_syntax) );
    ( "validate_lexeme - syntax check comes first" >:: fun _ ->
      (* Invalid syntax should fail even with valid dict *)
      assert (
        not (validate_lexeme "InvalidCat" "cat" sample_syntax (Some sample_dict)))
    );
    ( "validate_lexeme with None - empty string word" >:: fun _ ->
      assert (validate_lexeme "N" "" sample_syntax None) );
    ( "validate_constituent with NodeOne wrapping NodeOne" >:: fun _ ->
      (* Test NodeOne { t = NodeOne {...} } - line 31-33 in validator.ml *)
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let inner_node = NodeOne { t = leaf; category = "NP"; id = 2 } in
      let outer_node = NodeOne { t = inner_node; category = "NP"; id = 3 } in
      (* NP -> NP is not valid, so this should fail *)
      assert (not (validate_constituent outer_node "NP" sample_syntax)) );
    ( "validate_constituent with NodeOne wrapping NodeTwo" >:: fun _ ->
      (* Test NodeOne { t = NodeTwo {...} } - line 31-33 in validator.ml *)
      let leaf1 = Leaf { pair = create_lexeme "D" "the"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "N" "cat"; id = 2 } in
      let node_np = NodeOne { t = leaf2; category = "NP"; id = 3 } in
      let node_dp = NodeTwo { t1 = leaf1; t2 = node_np; category = "DP"; id = 4 } in
      let outer_node = NodeOne { t = node_dp; category = "DP"; id = 5 } in
      (* DP -> DP is not valid, so this should fail *)
      assert (not (validate_constituent outer_node "DP" sample_syntax)) );
    ( "validate_constituent with NodeTwo constituent + leaf (CL case)"
    >:: fun _ ->
      (* Test lines 49-61 in validator.ml: NodeTwo with constituent first, leaf second *)
      let leaf_n = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let node_np = NodeOne { t = leaf_n; category = "NP"; id = 2 } in
      let leaf_v = Leaf { pair = create_lexeme "V" "runs"; id = 3 } in
      (* This creates a VP with constituent NP on left, lexeme V on right *)
      let node_vp = NodeTwo { t1 = node_np; t2 = leaf_v; category = "VP"; id = 4 } in
      (* This should fail as VP doesn't have a rule for NP V *)
      assert (not (validate_constituent node_vp "VP" sample_syntax)) );
    ( "validate_constituent with NodeTwo using NodeTwo children" >:: fun _ ->
      (* Test lines 62-68: NodeTwo with two NodeTwo children *)
      let leaf1 = Leaf { pair = create_lexeme "V" "runs"; id = 1 } in
      let node_vp1 = NodeOne { t = leaf1; category = "VP"; id = 2 } in
      let leaf2 = Leaf { pair = create_lexeme "D" "the"; id = 3 } in
      let leaf3 = Leaf { pair = create_lexeme "N" "cat"; id = 4 } in
      let node_np = NodeOne { t = leaf3; category = "NP"; id = 5 } in
      let node_dp = NodeTwo { t1 = leaf2; t2 = node_np; category = "DP"; id = 6 } in
      let node_vp2 = NodeTwo { t1 = leaf1; t2 = node_dp; category = "VP"; id = 7 } in
      (* Create a structure with two NodeTwo children *)
      let combined = NodeTwo { t1 = node_vp1; t2 = node_vp2; category = "DP"; id = 8 } in
      (* This should fail as DP -> VP VP is not valid *)
      assert (not (validate_constituent combined "DP" sample_syntax)) );
  ]