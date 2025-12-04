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
        assert_equal "[Category: N | Word: boy] (id: 1)\n"
          (to_string_list (to_list t))
          ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "adding leaf node to empty list" >:: fun _ ->
      if validate_lexeme "N" "boy" sample_syntax (Some sample_dict) then
        let t, id1 = add empty (create_lexeme "N" "boy") in
        assert_equal "[Category: N | Word: boy] (id: 1)\n"
          (to_string_list (to_list t))
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
          "[Category: N | Word: boy] (id: 1)\n\
           [Category: V | Word: walks] (id: 2)\n"
          (to_string_list (to_list t))
          ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "testing exception for remove with empty list" >:: fun _ ->
      assert_raises Wrong (fun () -> remove empty 0) );
    ( "testing connect1 for leaf node" >:: fun _ ->
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
          "[NP[Category: N | Word: boy] (id: 1)] (id: 3)\n\
           [Category: V | Word: walks] (id: 2)\n"
          (to_string_list (to_list t))
          ~printer:string_of_string
      else assert_failure "Validation failed" );
    ( "testing connect2 for leaf and constituent node" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let t, id2 = add t (create_lexeme "V" "walks") in
      let node1 = get t id1 in
      let t, id3 = connect1 t node1 "NP" in
      let t, id4 = add t (create_lexeme "D" "the") in
      let node4 = get t id4 in
      let node3 = get t id3 in
      let t, id5 = connect2 t node4 node3 "DP" in
      assert_equal
        "[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] (id: \
         1)] (id: 3)] (id: 5)\n\
         [Category: V | Word: walks] (id: 2)\n"
        (to_string_list (to_list t))
        ~printer:string_of_string );
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
        "[TP[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] \
         (id: 1)] (id: 3)] (id: 5)[VP[Category: V | Word: walks] (id: 2)] (id: \
         6)] (id: 7)\n"
        (to_string_list (to_list t))
        ~printer:string_of_string );
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
        "[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] (id: \
         1)] (id: 3)] (id: 5)\n\
         [VP[Category: V | Word: walks] (id: 2)] (id: 6)\n"
        (to_string_list (to_list t))
        ~printer:string_of_string );
    ( "testing add for bar nodes" >:: fun _ ->
      let t, id1 = add empty (create_lexeme "N" "boy") in
      let node1 = get t id1 in
      let t, id2 = connect1 t node1 "N'" in
      let node2 = get t id2 in
      let t, id3 = connect1 t node2 "NP" in
      assert_equal "[NP[N'[Category: N | Word: boy] (id: 1)] (id: 2)] (id: 3)\n"
        (to_string_list (to_list t))
        ~printer:string_of_string );
  ]
