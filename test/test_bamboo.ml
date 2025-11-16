open OUnit2
open Test_tree
open Test_syntax
open Test_dictionary

let tests = "test suite" >::: tree_tests @ syntax_tests @ dictionary_tests
let _ = run_test_tt_main tests
