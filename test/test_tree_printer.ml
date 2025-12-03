open OUnit2
open Bamboo.Tree
open Bamboo.Dictionary
open Bamboo.Tree_printer

let tree_printer_tests =
  [
    ( "to_string with Leaf node" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let str = to_string leaf in
      assert_bool "Should contain category" (String.contains str 'N');
      assert_bool "Should be non-empty" (String.length str > 0) );
    ( "to_string with NodeOne" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let node1 = NodeOne { t = leaf; category = "NP"; id = 2 } in
      let str = to_string node1 in
      assert_bool "Should contain NP" (String.length str > 0);
      assert_bool "Should contain id" (String.contains str '2') );
    ( "to_string with NodeTwo" >:: fun _ ->
      let leaf1 = Leaf { pair = create_lexeme "D" "the"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "N" "cat"; id = 2 } in
      let node2 = NodeTwo { t1 = leaf1; t2 = leaf2; category = "DP"; id = 3 } in
      let str = to_string node2 in
      assert_bool "Should be non-empty" (String.length str > 0);
      assert_bool "Should contain DP" (String.contains str 'D') );
    ( "to_fancy_string with empty tree" >:: fun _ ->
      let str = to_fancy_string empty in
      assert_equal "" str ~printer:(fun s -> s) );
    ( "to_fancy_string with single leaf" >:: fun _ ->
      let t, _ = add empty (create_lexeme "N" "cat") in
      let str = to_fancy_string t in
      assert_bool "Should contain N" (String.contains str 'N');
      assert_bool "Should contain cat" (String.length str > 0) );
    ( "to_fancy_string with multiple leaves" >:: fun _ ->
      let t, _ = add empty (create_lexeme "N" "cat") in
      let t, _ = add t (create_lexeme "V" "runs") in
      let str = to_fancy_string t in
      assert_bool "Should have multiple lines" (String.contains str '\n');
      assert_bool "Should be non-empty" (String.length str > 0) );
    ( "to_fancy_string with NodeOne structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "cat") in
      let node1 = get t id1 in
      let t, _ = connect1 t node1 "NP" in
      let str = to_fancy_string t in
      assert_bool "Should contain NP" (String.contains str 'N');
      assert_bool "Should be multi-line" (String.contains str '\n') );
    ( "to_fancy_string with NodeTwo structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "the") in
      let t, id2 = add t (create_lexeme "N" "cat") in
      let node1 = get t id1 in
      let node2 = get t id2 in
      let node2_np = NodeOne { t = node2; category = "NP"; id = 99 } in
      let t, _ = connect2 t node1 node2_np "DP" in
      let str = to_fancy_string t in
      assert_bool "Should be non-empty for complex tree" (String.length str > 10)
    );
    ( "to_fancy_string_list with empty list" >:: fun _ ->
      let str = to_fancy_string_list [] in
      assert_equal "" str ~printer:(fun s -> s) );
    ( "to_fancy_string_list with single node" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let str = to_fancy_string_list [ leaf ] in
      assert_bool "Should contain content" (String.length str > 0) );
    ( "to_fancy_string_list with multiple nodes" >:: fun _ ->
      let leaf1 = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "V" "runs"; id = 2 } in
      let str = to_fancy_string_list [ leaf1; leaf2 ] in
      assert_bool "Should contain both nodes" (String.length str > 5) );
    ( "to_fancy_string with deeply nested structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "cat") in
      let node1 = get t id1 in
      let t, id2 = connect1 t node1 "NP" in
      let node2 = get t id2 in
      let t, _ = connect1 t node2 "DP" in
      let str = to_fancy_string t in
      assert_bool "Should handle nested structure" (String.length str > 0);
      assert_bool "Should have multiple levels" (String.contains str '\n') );
    ( "to_string with nested NodeOne in NodeTwo" >:: fun _ ->
      let leaf1 = Leaf { pair = create_lexeme "D" "the"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "N" "cat"; id = 2 } in
      let node_inner = NodeOne { t = leaf2; category = "NP"; id = 3 } in
      let node_outer =
        NodeTwo { t1 = leaf1; t2 = node_inner; category = "DP"; id = 4 }
      in
      let str = to_string node_outer in
      assert_bool "Should contain all IDs" (String.length str > 10) );
    ( "to_fancy_string_list with NodeOne" >:: fun _ ->
      let leaf = Leaf { pair = create_lexeme "N" "cat"; id = 1 } in
      let node1 = NodeOne { t = leaf; category = "NP"; id = 2 } in
      let str = to_fancy_string_list [ node1 ] in
      assert_bool "Should render NodeOne" (String.length str > 0) );
    ( "to_fancy_string_list with NodeTwo" >:: fun _ ->
      let leaf1 = Leaf { pair = create_lexeme "D" "the"; id = 1 } in
      let leaf2 = Leaf { pair = create_lexeme "N" "cat"; id = 2 } in
      let node2 = NodeTwo { t1 = leaf1; t2 = leaf2; category = "DP"; id = 3 } in
      let str = to_fancy_string_list [ node2 ] in
      assert_bool "Should render NodeTwo" (String.length str > 5) );
  ]
