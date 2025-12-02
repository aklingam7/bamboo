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
     N'\n\
     rules:\n\
     TP -> DP VP\n\
     NP -> N\n\
     DP -> D NP\n\
     VP -> V DP\n\
     VP -> V\n\
     NP -> N'\n\
     N' -> N\n"
(* Note that this is not of type [test] but of type [test list]. This is so that
   it is easy to add new tests here but combine all related tests together later
   in the combined test suite *)

let test_add name expected cat word syn dict trlst =
  add cat word syn dict trlst;
  name >:: fun _ ->
  assert_equal expected (to_string_list !trlst) ~printer:string_of_string

let test_remove name expected removal_id lst =
  remove removal_id lst;
  name >:: fun _ ->
  assert_equal expected (to_string_list !lst) ~printer:string_of_string

let test_connect2 name expected t1 t2 category syntax trlst =
  connect2 t1 t2 category syntax trlst;
  name >:: fun _ ->
  assert_equal expected (to_string_list !trlst) ~printer:string_of_string

let test_connect1 name expected tree const syn trlst =
  connect1 tree const syn trlst;
  name >:: fun _ ->
  assert_equal expected (to_string_list !trlst) ~printer:string_of_string

let tree_tests =
  [
    (let trlst7 = ref [] in
     let () = add "N" "boy" sample_syntax sample_dict trlst7 in
     let () = connect1 (get 26 !trlst7) "N'" sample_syntax trlst7 in
     test_connect1 "testing add for bar nodes"
       "[NP[N'[Category: N | Word: boy] (id: 26)] (id: 27)] (id: 28)\n"
       (get 27 !trlst7) "NP" sample_syntax trlst7);
    (let trlst6 = ref [] in
     let () = add "N" "boy" sample_syntax sample_dict trlst6 in
     let () = add "V" "walks" sample_syntax sample_dict trlst6 in
     let () = connect1 (get 19 !trlst6) "NP" sample_syntax trlst6 in
     let () = add "D" "the" sample_syntax sample_dict trlst6 in
     let () =
       connect2 (get 22 !trlst6) (get 21 !trlst6) "DP" sample_syntax trlst6
     in
     let () = connect1 (get 20 !trlst6) "VP" sample_syntax trlst6 in
     let () =
       connect2 (get 23 !trlst6) (get 24 !trlst6) "TP" sample_syntax trlst6
     in
     test_remove "testing remove for a branching node"
       "[DP[Category: D | Word: the] (id: 22)[NP[Category: N | Word: boy] (id: \
        19)] (id: 21)] (id: 23)\n\
        [VP[Category: V | Word: walks] (id: 20)] (id: 24)\n"
       25 trlst6);
    (let trlst5 = ref [] in
     let () = add "N" "boy" sample_syntax sample_dict trlst5 in
     let () = add "V" "walks" sample_syntax sample_dict trlst5 in
     let () = connect1 (get 12 !trlst5) "NP" sample_syntax trlst5 in
     let () = add "D" "the" sample_syntax sample_dict trlst5 in
     let () =
       connect2 (get 15 !trlst5) (get 14 !trlst5) "DP" sample_syntax trlst5
     in
     let () = connect1 (get 13 !trlst5) "VP" sample_syntax trlst5 in
     test_connect2 "testing connect2 for two constituent nodes"
       "[TP[DP[Category: D | Word: the] (id: 15)[NP[Category: N | Word: boy] \
        (id: 12)] (id: 14)] (id: 16)[VP[Category: V | Word: walks] (id: 13)] \
        (id: 17)] (id: 18)\n"
       (get 16 !trlst5) (get 17 !trlst5) "TP" sample_syntax trlst5);
    (let trlst4 = ref [] in
     let () = add "N" "boy" sample_syntax sample_dict trlst4 in
     let () = add "V" "walks" sample_syntax sample_dict trlst4 in
     let () = connect1 (get 7 !trlst4) "NP" sample_syntax trlst4 in
     let () = add "D" "the" sample_syntax sample_dict trlst4 in
     test_connect2 "testing connect2 for leaf and constituent node"
       "[DP[Category: D | Word: the] (id: 10)[NP[Category: N | Word: boy] (id: \
        7)] (id: 9)] (id: 11)\n\
        [Category: V | Word: walks] (id: 8)\n"
       (get 10 !trlst4) (get 9 !trlst4) "DP" sample_syntax trlst4);
    (let trlst3 = ref [] in
     let () = add "N" "boy" sample_syntax sample_dict trlst3 in
     let () = add "V" "walks" sample_syntax sample_dict trlst3 in
     test_connect1 "testing connect1 for leaf node"
       "[NP[Category: N | Word: boy] (id: 4)] (id: 6)\n\
        [Category: V | Word: walks] (id: 5)\n"
       (get 4 !trlst3) "NP" sample_syntax trlst3);
    ( "testing exception for remove with empty list" >:: fun _ ->
      assert_raises Wrong (fun () -> remove 0 treelist) );
    (let trlst2 = ref [] in
     let () = add "N" "boy" sample_syntax sample_dict trlst2 in
     test_add "adding leaf node to non-empty list"
       "[Category: N | Word: boy] (id: 2)\n\
        [Category: V | Word: walks] (id: 3)\n"
       "V" "walks" sample_syntax sample_dict trlst2);
    (let trlst1 = ref [] in
     test_add "adding leaf node to empty list"
       "[Category: N | Word: boy] (id: 1)\n" "N" "boy" sample_syntax sample_dict
       trlst1);
  ]

(*let tree_tests = ignore [ ( "testing exception for remove with empty list" >::
  fun _ -> assert_raises Wrong (fun () -> remove 0 !treelist) ); ]; add "N"
  "boy" sample_syntax sample_dict;

  ignore [ ( "testing single leaf node" >:: fun _ -> assert_equal "[Category: N
  | Word: boy] (id: 1)\n" (to_string_list !treelist) ~printer:string_of_string
  ); ];

  add "V" "walks" sample_syntax sample_dict;

  ignore [ ( "testing multiple leaf nodes" >:: fun _ -> assert_equal "[Category:
  N | Word: boy] (id: 1)\n\ [Category: V | Word: walks] (id: 2)\n"
  (to_string_list !treelist) ~printer:string_of_string ); ( "testing get
  function" >:: fun _ -> assert_equal "[Category: N | Word: boy] (id: 1)"
  (to_string (get 1 !treelist)) ); ];

  connect1 (get 1 !treelist) "NP" sample_syntax; ignore [ ( "testing leaf node
  connect" >:: fun _ -> assert_equal "[NP[Category: N | Word: boy] (id: 1)] (id:
  3)\n\ [Category: V | Word: walks] (id: 2)\n" (to_string_list !treelist)
  ~printer:string_of_string ); ];

  add "D" "the" sample_syntax sample_dict;

  connect2 (get 4 !treelist) (get 3 !treelist) "DP" sample_syntax; ignore [ (
  "testing non-leaf node connect" >:: fun _ -> assert_equal "[DP[Category: D |
  Word: the] (id: 4)[NP[Category: N | Word: boy] \ (id: 1)] (id: 3)] (id: 5)\n\
  [Category: V | Word: walks] (id: 2)\n" (to_string_list !treelist)
  ~printer:string_of_string ); ];

  remove 5 !treelist;

  ignore [ ( "testing non-leaf node connect" >:: fun _ -> assert_equal
  "[Category: D | Word: the] (id: 4)\n\ [NP[Category: N | Word: boy] (id: 1)]
  (id: 3)\n\ [Category: V | Word: walks] (id: 2)\n" (to_string_list !treelist)
  ~printer:string_of_string ); ];

  connect2 (get 4 !treelist) (get 3 !treelist) "DP" sample_syntax;

  connect1 (get 2 !treelist) "VP" sample_syntax;

  connect2 (get 6 !treelist) (get 7 !treelist) "TP" sample_syntax;

  ignore [ ( "testing\n non-leaf node connect" >:: fun _ -> assert_equal
  "[TP[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy] \ (id:
  1)] (id: 3)] (id: 6)[VP[Category: V | Word: walks] (id: 2)] \ (id: 7)] (id:
  8)\n" (to_string_list !treelist) ~printer:string_of_string ); ];

  remove 8 !treelist; [ ( "testing\n branching node remove" >:: fun _ ->
  assert_equal "[DP[Category: D | Word: the] (id: 4)[NP[Category: N | Word: boy]
  (id: \ 1)] (id: 3)] (id: 6)\n\ [VP[Category: V | Word: walks] (id: 2)] (id:
  7)\n" (to_string_list !treelist) ~printer:string_of_string );

  ( "testing exception for remove" >:: fun _ -> assert_raises Wrong (fun () ->
  add "N" "toy" sample_syntax sample_dict) );

  ( "testing exception for connect1" >:: fun _ -> assert_raises Wrong (fun () ->
  connect1 (get 0 !treelist) "DP" sample_syntax) );

  ( "testing exception for remove with empty list" >:: fun _ -> assert_raises
  Wrong (fun () -> remove 0 !treelist) ); ]*)
