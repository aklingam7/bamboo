open OUnit2
open Test_tree
open Test_rules
open Test_word

let tests = "test suite" >::: tree_tests @ rules_tests @ word_tests
let _ = run_test_tt_main tests
