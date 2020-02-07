open OUnit2
(* If you get an "unbound module" error from the line below,
   it's most likely because you have not (re)compiled [enigma.ml]. 
   To do that, run [make build]. *)
open Enigma

(** [make_index_test name input expected_output] constructs an OUnit
    test named [name] that asserts the quality of [expected_output]
    with [index input]. *)
let make_index_test 
    (name : string) 
    (input: char) 
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      (* the [printer] tells OUnit how to convert the output to a string *)
      assert_equal expected_output (index input) ~printer:string_of_int)

(* TODO: you will find it helpful to write functions like [make_index_test]
   for each of the other functions you are testing.  They will keep your
   lists of tests below very readable, and will also help you to avoid
   repeating code. *)

let index_tests = [
  make_index_test "index of A is 0" 'A' 0;
  (* TODO: add your tests here *)
]

let map_rl_tests = [
  (* TODO: add your tests here *)
]

let map_lr_tests = [
  (* TODO: add your tests here *)
]

let map_refl_tests = [
  (* TODO: add your tests here *)
]

let map_plug_tests = [
  (* TODO: add your tests here *)
]

let cipher_char_tests = [
  (* TODO: add your tests here *)
]

let step_tests = [
  (* TODO: add your tests here *)
]

let cipher_tests = [
  (* TODO: add your tests here *)
]

let tests =
  "test suite for A1"  >::: List.flatten [
    index_tests;
    map_rl_tests;
    map_lr_tests;
    map_refl_tests;
    map_plug_tests;
    cipher_char_tests;
    step_tests;
    cipher_tests;
  ]

let _ = run_test_tt_main tests
