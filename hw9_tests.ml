(*
   Name: 
   File: hw9_tests.ml
   Date: Spring 2023
   Desc: Unit tests for HW9
*)

open Hw9

let msg = "--- Running HW8 Tests --- ";;
print_endline msg;;

(* equality assertions *)
let assert_equal v1 v2 msg =
  let cond = v1 = v2 in
  assert (if not cond then print_endline ("TEST FAILED: " ^ msg) ; cond)
;;


