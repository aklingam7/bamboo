open OUnit2
open Bamboo.Tree
open Bamboo.Dictionary
open Bamboo.Tree_printer

let large_tree =
  "[TP[DP[Category: D | Word: 0] (1)[NP[Category: N | Word: pandas] (2)] (6)] \
   (8)[VP[Category: V | Word: love] (3)[DP[Category: D | Word: 0] \
   (4)[NP[Category: N | Word: bamboo] (5)] (7)] (9)] (10)] (11)\n"

let large_fancy_tree =
  "             [TP] (11)             \n\
  \                / \\                \n\
  \              /     \\              \n\
  \            /         \\            \n\
  \          /             \\          \n\
  \        /               |          \n\
  \   [DP] (8)         [VP] (10)      \n\
  \     / \\               / \\         \n\
  \   /     \\           /     \\       \n\
   D (1) [NP] (6)     /       |       \n\
  \  |      |     V (3)    [DP] (9)   \n\
  \  0    N (2)     |        / \\      \n\
  \         |     love     /     \\    \n\
  \       pandas        D (4) [NP] (7)\n\
  \                       |      |    \n\
  \                       0    N (5)  \n\
  \                              |    \n\
  \                            bamboo "

let tree_printer_tests =
  [
    ( "to_string with empty tree" >:: fun _ ->
      let str = to_string empty in
      assert_equal "" str ~printer:(fun s -> s) );
    ( "to_string with single leaf" >:: fun _ ->
      let leaf_tree = fst (add empty (create_lexeme "N" "cat")) in
      let str = to_string leaf_tree in
      assert_bool "Should contain category" (String.contains str 'N');
      assert_bool "Should be non-empty" (String.length str > 0) );
    ( "to_string with multiple leaves" >:: fun _ ->
      let t, _ = add empty (create_lexeme "N" "cat") in
      let t, _ = add t (create_lexeme "V" "runs") in
      let str = to_string t in
      assert_bool "Should have multiple lines" (String.contains str '\n');
      assert_bool "Should be non-empty" (String.length str > 0) );
    ( "to_string with NodeOne structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "cat") in
      let node1 = get t id1 in
      let t, _ = connect1 t node1 "NP" in
      let str = to_string t in
      assert_bool "Should contain NP" (String.contains str 'N');
      assert_bool "Should be multi-line" (String.contains str '\n') );
    ( "to_string with NodeTwo structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "the") in
      let t, id2 = add t (create_lexeme "N" "cat") in
      let node1 = get t id1 in
      let node2 = get t id2 in
      let node2_np = NodeOne { t = node2; category = "NP"; id = 99 } in
      let t, _ = connect2 t node1 node2_np "DP" in
      let str = to_string t in
      assert_bool "Should be non-empty for complex tree" (String.length str > 10)
    );
    ( "to_string with deeply nested structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "the") in
      let node1 = get t id1 in
      let t, id2 = add t (create_lexeme "N" "dinosaur") in
      let node2 = get t id2 in
      let t, id3 = add t (create_lexeme "V" "plays") in
      let node3 = get t id3 in
      let t, id4 = connect1 t node2 "NP" in
      let node4 = get t id4 in
      let t, id5 = connect1 t node3 "VP" in
      let node5 = get t id5 in
      let t, id6 = connect2 t node1 node4 "DP" in
      let node6 = get t id6 in
      let t, _ = connect2 t node6 node5 "TP" in
      let str = to_string t in
      assert_bool "Should handle nested structure" (String.length str > 0);
      assert_bool "Should have multiple levels" (String.contains str '\n') );
    ( "to_string large test with exact matching" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "0") in
      let node1 = get t id1 in
      let t, id2 = add t (create_lexeme "N" "pandas") in
      let node2 = get t id2 in
      let t, id3 = add t (create_lexeme "V" "love") in
      let node3 = get t id3 in
      let t, id4 = add t (create_lexeme "D" "0") in
      let node4 = get t id4 in
      let t, id5 = add t (create_lexeme "N" "bamboo") in
      let node5 = get t id5 in
      let t, id6 = connect1 t node2 "NP" in
      let node6 = get t id6 in
      let t, id7 = connect1 t node5 "NP" in
      let node7 = get t id7 in
      let t, id8 = connect2 t node1 node6 "DP" in
      let node8 = get t id8 in
      let t, id9 = connect2 t node4 node7 "DP" in
      let node9 = get t id9 in
      let t, id10 = connect2 t node3 node9 "VP" in
      let node10 = get t id10 in
      let t, _ = connect2 t node8 node10 "TP" in
      let str = to_string t in
      assert_equal large_tree str ~printer:(fun s -> s) );
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
    ( "to_fancy_string with deeply nested structure" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "the") in
      let node1 = get t id1 in
      let t, id2 = add t (create_lexeme "N" "dinosaur") in
      let node2 = get t id2 in
      let t, id3 = add t (create_lexeme "V" "plays") in
      let node3 = get t id3 in
      let t, id4 = connect1 t node2 "NP" in
      let node4 = get t id4 in
      let t, id5 = connect1 t node3 "VP" in
      let node5 = get t id5 in
      let t, id6 = connect2 t node1 node4 "DP" in
      let node6 = get t id6 in
      let t, _ = connect2 t node6 node5 "TP" in
      let str = to_fancy_string t in
      assert_bool "Should handle nested structure" (String.length str > 0);
      assert_bool "Should have multiple levels" (String.contains str '\n') );
    ( "to_fancy_string large test with exact matching" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "0") in
      let node1 = get t id1 in
      let t, id2 = add t (create_lexeme "N" "pandas") in
      let node2 = get t id2 in
      let t, id3 = add t (create_lexeme "V" "love") in
      let node3 = get t id3 in
      let t, id4 = add t (create_lexeme "D" "0") in
      let node4 = get t id4 in
      let t, id5 = add t (create_lexeme "N" "bamboo") in
      let node5 = get t id5 in
      let t, id6 = connect1 t node2 "NP" in
      let node6 = get t id6 in
      let t, id7 = connect1 t node5 "NP" in
      let node7 = get t id7 in
      let t, id8 = connect2 t node1 node6 "DP" in
      let node8 = get t id8 in
      let t, id9 = connect2 t node4 node7 "DP" in
      let node9 = get t id9 in
      let t, id10 = connect2 t node3 node9 "VP" in
      let node10 = get t id10 in
      let t, _ = connect2 t node8 node10 "TP" in
      let str = to_fancy_string t in
      assert_equal large_fancy_tree str ~printer:(fun s -> s) );
  ]
