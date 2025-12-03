open OUnit2
open Test_tree
open Test_syntax
open Test_dictionary
open Test_validator
open Test_tree_printer

let tests =
  "test suite"
  >::: tree_tests @ syntax_tests @ dictionary_tests @ validator_tests
       @ tree_printer_tests

let _ = run_test_tt_main tests
