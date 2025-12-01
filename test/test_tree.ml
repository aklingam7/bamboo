open OUnit2
open Bamboo.Tree
open Bamboo.Syntax
open Bamboo.Dictionary

let string_of_string str = str
let string_of_unit () = ""

let sample_dict =
  build_dictionary [ [ "N"; "boy" ]; [ "V"; "walks" ]; [ "D"; "the" ] ]

let rec to_string_list treelst =
  match treelst with
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
     rules:\n\
     TP -> DP VP\n\
     NP -> N\n\
     DP -> D NP\n\
     VP -> V DP\n\
     VP -> V\n"
(* Note that this is not of type [test] but of type [test list]. This is so that
   it is easy to add new tests here but combine all related tests together later
   in the combined test suite *)

let test_add name expected cat word syn dict =
  name >:: fun _ ->
  assert_equal expected (add cat word syn dict) ~printer:string_of_unit

let test_connect1 name expected tree const syn =
  name >:: fun _ ->
  assert_equal expected (connect1 tree const syn) ~printer:string_of_unit

let tree_tests =
  add "N" "boy" sample_syntax sample_dict;
  ignore
    [
      ( "testing single leaf node" >:: fun _ ->
        assert_equal "[Category: N | Word: boy] (id: 1)\n"
          (to_string_list !treelist) ~printer:string_of_string );
      (*test_connect1 "connect N to NP" () (get 0 !treelist) "NP"
        sample_syntax;*)
    ];
  add "V" "walks" sample_syntax sample_dict;
  ignore
    [
      ( "testing multiple leaf nodes" >:: fun _ ->
        assert_equal
          "[Category: N | Word: boy] (id: 1)\n\
           [Category: V | Word: walks] (id: 2)\n"
          (to_string_list !treelist) ~printer:string_of_string );
      (*test_connect1 "connect N to NP" () (get 0 !treelist) "NP"
        sample_syntax;*)
      ( "testing get function" >:: fun _ ->
        assert_equal "[Category: N | Word: boy] (id: 1)"
          (to_string (get 1 !treelist)) );
    ];
  connect1 (get 1 !treelist) "NP" sample_syntax;
  ignore
    [
      ( "testing leaf node connect" >:: fun _ ->
        assert_equal
          "[NP[Category: N | Word: boy] (id: 1)] (id: 3)\n\
           [Category: V | Word: walks] (id: 2)\n"
          (to_string_list !treelist) ~printer:string_of_string );
      (*test_connect1 "connect N to NP" () (get 0 !treelist) "NP"
        sample_syntax;*)
    ];
  add "D" "the" sample_syntax sample_dict;
  connect2 (get 4 !treelist) (get 3 !treelist) "DP" sample_syntax;
  ignore
    [
      ( "testing non-leaf node connect" >:: fun _ ->
        assert_equal
          "[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] \
           (id: 1)] (id: 3)] (id: 5)\n\
           [Category: V | Word: walks] (id: 2)\n"
          (to_string_list !treelist) ~printer:string_of_string );
      (*test_connect1 "connect N to NP" () (get 0 !treelist) "NP"
        sample_syntax;*)
    ];
  remove 5 !treelist;
  ignore
    [
      ( "testing non-leaf node connect" >:: fun _ ->
        assert_equal
          "[Category: D | Word: the] (id: 4)\n\
           [NP[Category: N | Word: boy] (id: 1)] (id: 3)\n\
           [Category: V | Word: walks] (id: 2)\n"
          (to_string_list !treelist) ~printer:string_of_string );
    ];
  connect2 (get 4 !treelist) (get 3 !treelist) "DP" sample_syntax;
  connect1 (get 2 !treelist) "VP" sample_syntax;
  connect2 (get 6 !treelist) (get 7 !treelist) "TP" sample_syntax;
  ignore
    [
      ( "testing\n  non-leaf node connect" >:: fun _ ->
        assert_equal
          "[TP[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] \
           (id: 1)] (id: 3)] (id: 6)[VP[Category: V | Word: walks] (id: 2)] \
           (id: 7)] (id: 8)\n"
          (to_string_list !treelist) ~printer:string_of_string );
    ];
  remove 8 !treelist;
  [
    ( "testing\n  branching node remove" >:: fun _ ->
      assert_equal
        "[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] (id: \
         1)] (id: 3)] (id: 6)\n\
         [VP[Category: V | Word: walks] (id: 2)] (id: 7)\n"
        (to_string_list !treelist) ~printer:string_of_string );
  ]
