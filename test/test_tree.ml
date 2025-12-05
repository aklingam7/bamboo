open OUnit2
open Bamboo.Tree
open Bamboo.Syntax
open Bamboo.Dictionary
open Bamboo.Tree_printer
open Bamboo.Validator

let string_of_string str = str

let sample_dict =
  build_dictionary [ [ "N"; "boy" ]; [ "V"; "walks" ]; [ "D"; "the" ] ]

let rec to_string_list nodes =
  match nodes with
  | [] -> ""
  | h :: t -> to_string h ^ "\n" ^ to_string_list t

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
     N'\n\
     rules:\n\
     TP -> DP VP\n\
     NP -> N\n\
     DP -> D NP\n\
     VP -> V DP\n\
     VP -> V\n\
     NP -> N'\n\
     N' -> N\n"

let assert_trees_equal (t1 : tree) (t2 : tree) =
  let rec assert_tree_nodes_equal n1 n2 =
    match (n1, n2) with
    | Leaf { pair = pair1; id = id1 }, Leaf { pair = pair2; id = id2 } ->
        assert_equal (get_lexeme_category pair1) (get_lexeme_category pair2)
          ~printer:(fun s -> s);
        assert_equal (get_lexeme_word pair1) (get_lexeme_word pair2)
          ~printer:(fun s -> s);
        assert_equal id1 id2 ~printer:(fun i -> string_of_int i)
    | ( NodeOne { t = t1; category = category1; id = id1 },
        NodeOne { t = t2; category = category2; id = id2 } ) ->
        assert_equal category1 category2 ~printer:(fun s -> s);
        assert_equal id1 id2 ~printer:(fun i -> string_of_int i);
        assert_tree_nodes_equal t1 t2
    | ( NodeTwo { t1 = t11; t2 = t12; category = category1; id = id1 },
        NodeTwo { t1 = t21; t2 = t22; category = category2; id = id2 } ) ->
        assert_equal category1 category2 ~printer:(fun s -> s);
        assert_equal id1 id2 ~printer:(fun i -> string_of_int i);
        assert_tree_nodes_equal t11 t21;
        assert_tree_nodes_equal t12 t22
    | _ -> assert_failure "Different node types"
  in
  let t1, t2 = (to_list t1, to_list t2) in
  List.iter2 assert_tree_nodes_equal t1 t2

let tree_tests =
  [
    ( "adding leaf nodes, removing both, and adding another one" >:: fun _ ->
      if
        validate_lexeme "N" "boy" sample_syntax (Some sample_dict)
        && validate_lexeme "V" "walks" sample_syntax (Some sample_dict)
      then
        let t, id1 = add empty (create_lexeme "N" "boy") in
        let t, id2 = add t (create_lexeme "V" "walks") in
        let t = remove t id1 in
        let t = remove t id2 in
        let t, id3 = add t (create_lexeme "N" "boy") in
        assert_equal "[Category: N | Word: boy] (1)\n" (to_string t)
          ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "adding leaf node to empty list" >:: fun _ ->
      if validate_lexeme "N" "boy" sample_syntax (Some sample_dict) then
        let t, id1 = add empty (create_lexeme "N" "boy") in
        assert_equal "[Category: N | Word: boy] (1)\n" (to_string t)
          ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "adding leaf node to non-empty list" >:: fun _ ->
      if
        validate_lexeme "N" "boy" sample_syntax (Some sample_dict)
        && validate_lexeme "V" "walks" sample_syntax (Some sample_dict)
      then
        let t, id1 = add empty (create_lexeme "N" "boy") in
        let t, id2 = add t (create_lexeme "V" "walks") in
        assert_equal
          "[Category: N | Word: boy] (1)\n[Category: V | Word: walks] (2)\n"
          (to_string t) ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "testing remove for a nonexistent node" >:: fun _ ->
      assert_raises (NonTopLevelNodeID 0) (fun () -> remove empty 0) );
    ( "testing remove for a top-level node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "friend") in
      assert_trees_equal Bamboo.Tree.empty (remove t id1) );
    ( "testing remove for a nested node" >:: fun _ ->
      let lexeme = create_lexeme "D" "0" in
      let t, id1 = add empty lexeme in
      let node1 = get t id1 in
      let t, _ = connect1 t node1 "NP" in
      assert_raises (NonTopLevelNodeID 1) (fun () -> remove t 1) );
    ( "testing remove for a deeply nested node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "food") in
      let node1 = get t id1 in
      let lexeme = create_lexeme "Adj" "good" in
      let t, id2 = add t lexeme in
      let node2 = get t id2 in
      let t, id3 = add t (create_lexeme "D" "the") in
      let node3 = get t id3 in
      let t, id4 = connect1 t node1 "NP" in
      let node4 = get t id4 in
      let t, id5 = connect2 t node2 node4 "NP" in
      let node5 = get t id5 in
      let t, id6 = connect2 t node3 node5 "DP" in
      assert_raises (NonTopLevelNodeID id2) (fun () -> remove t id2) );
    ( "testing remove for a single-child node" >:: fun _ ->
      let t1, id1 = add empty (create_lexeme "N" "color") in
      let node1 = get t1 id1 in
      let t2, id2 = connect1 t1 node1 "NP" in
      assert_trees_equal t1 (remove t2 id2) );
    ( "testing remove for a branching node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let t, id2 = add t (create_lexeme "V" "walks") in
      let node1 = get t id1 in
      let t, id3 = connect1 t node1 "NP" in
      let t, id4 = add t (create_lexeme "D" "the") in
      let node4 = get t id4 in
      let node3 = get t id3 in
      let t, id5 = connect2 t node4 node3 "DP" in
      let node2 = get t id2 in
      let t, id6 = connect1 t node2 "VP" in
      let node5 = get t id5 in
      let node6 = get t id6 in
      let t, id7 = connect2 t node5 node6 "TP" in
      let t = remove t id7 in
      assert_equal
        "[DP[Category: D | Word: the] (4)[NP[Category: N | Word: boy] (1)] \
         (3)] (5)\n\
         [VP[Category: V | Word: walks] (2)] (6)\n"
        (to_string t) ~printer:string_of_string );
    ( "testing remove of a node with a branching grandchild" >:: fun _ ->
      let t1, id1 = add empty (create_lexeme "V" "love") in
      let node1 = get t1 id1 in
      let t2, id2 = add t1 (create_lexeme "D" "the") in
      let node2 = get t2 id2 in
      let t3, id3 = add t2 (create_lexeme "Adj" "pretty") in
      let node3 = get t3 id3 in
      let t4, id4 = add t3 (create_lexeme "N" "colours") in
      let node4 = get t4 id4 in
      let t5, id5 = connect1 t4 node4 "NP" in
      let node5 = get t5 id5 in
      let t6, id6 = connect2 t5 node3 node5 "NP" in
      let node6 = get t6 id6 in
      let t7, id7 = connect2 t6 node2 node6 "DP" in
      let node7 = get t7 id7 in
      let t8, id8 = connect2 t7 node1 node7 "VP" in
      assert_trees_equal t7 (remove t8 id8) );
    ( "testing get for a nonexistent node" >:: fun _ ->
      assert_raises (NonexistentNodeID 1) (fun () -> get empty 1) );
    ( "testing get for a top-level node" >:: fun _ ->
      let lexeme = create_lexeme "D" "0" in
      let t, id1 = add empty lexeme in
      let node1 = get t id1 in
      assert_equal
        (Leaf { pair = lexeme; id = 1 })
        node1 ~printer:string_of_tree_node );
    ( "testing get for a nested node" >:: fun _ ->
      let lexeme = create_lexeme "D" "0" in
      let t, id1 = add empty lexeme in
      let node1 = get t id1 in
      let t, _ = connect1 t node1 "NP" in
      let node1 = get t id1 in
      assert_equal
        (Leaf { pair = lexeme; id = 1 })
        node1 ~printer:string_of_tree_node );
    ( "testing get for a deeply nested node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "food") in
      let node1 = get t id1 in
      let lexeme = create_lexeme "Adj" "good" in
      let t, id2 = add t lexeme in
      let node2 = get t id2 in
      let t, id3 = add t (create_lexeme "D" "the") in
      let node3 = get t id3 in
      let t, id4 = connect1 t node1 "NP" in
      let node4 = get t id4 in
      let t, id5 = connect2 t node2 node4 "NP" in
      let node5 = get t id5 in
      let t, id6 = connect2 t node3 node5 "DP" in
      assert_equal
        (Leaf { pair = lexeme; id = 2 })
        (get t id2) ~printer:string_of_tree_node );
    ( "testing connect1 for a leaf node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let t, id2 = add t (create_lexeme "V" "walks") in
      let node1 = get t id1 in
      if
        validate_constituent
          (NodeOne { t = node1; category = "NP"; id = 0 })
          "NP" sample_syntax
      then
        let t, id3 = connect1 t node1 "NP" in
        assert_equal
          "[NP[Category: N | Word: boy] (1)] (3)\n\
           [Category: V | Word: walks] (2)\n"
          (to_string t) ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "testing connect2 for a leaf and a constituent node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let t, id2 = add t (create_lexeme "V" "walks") in
      let node1 = get t id1 in
      let t, id3 = connect1 t node1 "NP" in
      let t, id4 = add t (create_lexeme "D" "the") in
      let node4 = get t id4 in
      let node3 = get t id3 in
      let t, id5 = connect2 t node4 node3 "DP" in
      assert_equal
        "[Category: V | Word: walks] (2)\n\
         [DP[Category: D | Word: the] (4)[NP[Category: N | Word: boy] (1)] \
         (3)] (5)\n"
        (to_string t) ~printer:string_of_string );
    ( "testing connect2 for two constituent nodes" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let t, id2 = add t (create_lexeme "V" "walks") in
      let node1 = get t id1 in
      let t, id3 = connect1 t node1 "NP" in
      let t, id4 = add t (create_lexeme "D" "the") in
      let node4 = get t id4 in
      let node3 = get t id3 in
      let t, id5 = connect2 t node4 node3 "DP" in
      let node2 = get t id2 in
      let t, id6 = connect1 t node2 "VP" in
      let node5 = get t id5 in
      let node6 = get t id6 in
      let t, id7 = connect2 t node5 node6 "TP" in
      assert_equal
        "[TP[DP[Category: D | Word: the] (4)[NP[Category: N | Word: boy] (1)] \
         (3)] (5)[VP[Category: V | Word: walks] (2)] (6)] (7)\n"
        (to_string t) ~printer:string_of_string );
    ( "testing add for bar nodes" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let node1 = get t id1 in
      let t, id2 = connect1 t node1 "N'" in
      let node2 = get t id2 in
      let t, id3 = connect1 t node2 "NP" in
      assert_equal "[NP[N'[Category: N | Word: boy] (1)] (2)] (3)\n"
        (to_string t) ~printer:string_of_string );
    ( "testing string_of_tree_node (Leaf)" >:: fun _ ->
      let lexeme = create_lexeme "N" "boy" in
      assert_equal "(id 2 -> Category: N | Word: boy)"
        (string_of_tree_node (Leaf { pair = lexeme; id = 2 }))
        ~printer:string_of_string );
    ( "testing string_of_tree_node (NodeOne)" >:: fun _ ->
      let t, id = add empty (create_lexeme "N" "boy") in
      let node = get t id in
      assert_equal "(id 0 -> NP - (id 1 -> Category: N | Word: boy))"
        (string_of_tree_node (NodeOne { t = node; category = "NP"; id = 0 }))
        ~printer:string_of_string );
    ( "testing string_of_tree_node (NodeTwo)" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "D" "the") in
      let t, id2 = add t (create_lexeme "N" "boy") in
      let node1 = get t id1 in
      let node2 = get t id2 in
      assert_equal
        "(id 0 -> NP - 1((id 1 -> Category: D | Word: the) - 2(id 2 -> \
         Category: N | Word: boy))"
        (string_of_tree_node
           (NodeTwo { t1 = node1; t2 = node2; category = "NP"; id = 0 }))
        ~printer:string_of_string );
  ]
