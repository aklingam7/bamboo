open OUnit2

(* Note that this is not of type [test] but of type [test list]. This is so that
   it is easy to add new tests here but combine all related tests together later
   in the combined test suite *)
let syntax_tests = [ ("a trivial test" >:: fun _ -> assert_equal 0 0) ]
