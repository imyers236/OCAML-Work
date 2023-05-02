(*
   Name: Ian Myers
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

(* Question 1: my_last tests *)
assert_equal 3 (my_last [1; 2; 3]) "3 = my_last [1; 2; 3]";;
assert_equal 'd' (my_last ['a'; 'b'; 'c'; 'd']) "'d' = my_last ['a'; 'b'; 'c'; 'd']";;

(* Question 2: my_init tests *)
assert_equal [1;2] (my_init [1; 2; 3]) "[1;2] = my_init [1; 2; 3]";;
assert_equal [1;2;3;3;2] (my_init [1; 2; 3; 3; 2; 1]) "[1;2;3;3;2] = my_init [1; 2; 3; 3; 2; 1]";;

(* Question 3: my_replace tests *)
assert_equal [1;2;5] (my_replace (3,5) [1;2;3]) "[1;2;5] = my_replace (3,5) [1;2;3]";;
assert_equal [1;2;3] (my_replace (6,5) [1;2;3]) "[1;2;3] = my_replace (6,5) [1;2;3]";;
assert_equal [5;5;5] (my_replace (2,5) [2;2;2]) "[5;5;5] = my_replace (2,5) [2;2;2]";;

(* Question 4: my_replace_all tests *)
assert_equal [2;2;4;4] (my_replace_all [(1,2);(3,4)] [1;2;3;4]) "[2;2;4;4] = my_replace_all [(1,2);(3,4)] [1;2;3;4]";;
assert_equal [1;2;4;5;5] (my_replace_all [(6,5);(3,4)] [1;2;3;6;5]) "[1;2;4;5;5] = my_replace_all (6,5) [1;2;3]";;
assert_equal ['b';'b';'d';'d'] (my_replace_all [('a','b');('c','d')] ['a';'b';'c';'d']) "['b';'b';'d';'d'] = my_replace_all [('a','b');('c','d')] ['a';'b';'c';'d']";;

(* Question 5: my_elem_sum tests *)
assert_equal 3 (my_elem_sum 3 [1;2;3]) "3 = my_elem_sum 3 [1;2;3]";;
assert_equal 0 (my_elem_sum 5 [1;2;3]) "0 = my_elem_sum 5 [1;2;3]";;
assert_equal 10 (my_elem_sum 5 [5;2;5]) "10 = my_elem_sum 5 [5;2;5]";;

(* Question 6: my_range_sum tests *)
assert_equal 3 (my_range_sum 1 2) "3 = my_range_sum 1 2";;
assert_equal 0 (my_range_sum 5 2) "0 = my_range_sum 5 2";;
assert_equal 30 (my_range_sum 4 8) "30 = my_range_sum 4 8";;

(* Question 7: insert tests *)
assert_equal (Node ('a', 1, Nil)) (insert 'a' 1 Nil) "insert 'a' 1 Nil";;
assert_equal (Node  ('a', 1, Node ('b', 1, Nil))) (insert 'a' 1 (Node ('b', 1, Nil))) "insert 'a' 1 (Node ('b', 1, Nil))";;
assert_equal (Node ('c', 2, (Node  ('a', 1, Node ('b', 1, Nil))))) (insert 'c' 2 (Node ('a', 1, (Node ('b', 1, Nil))))) "insert 'c' 2 (Node ('a' 1 (Node ('b', 1, Nil))))";;

(* Question 8: remove tests *)
assert_equal (Nil) (remove 'a' Nil) "remove 'a' Nil";;
assert_equal (Nil) (remove 'a' (Node ('a', 1, Nil))) "remove 'a' (Node ('a', 1, Nil))";;
assert_equal (Nil) (remove 'a' (Node ('a', 1, Node ('a', 2, Nil)))) "remove 'a' (Node ('a', 1, Node ('a', 2, Nil)))";;
assert_equal (Node ('b', 1, Nil)) (remove 'a' (Node ('b', 1, Node ('a', 2, Nil)))) "remove 'a' (Node ('b', 1, Node ('a', 2, Nil)))";;

(* Question 9: size tests *)
assert_equal (0) (size Nil) "size Nil";;
assert_equal (2) (size (Node ('a', 1, Node ('b', 1, Nil)))) "size (Node ('a', 1, Node ('b', 1, Nil)))";;
assert_equal (3) (size (Node ('c', 2, (Node  ('a', 1, Node ('b', 1, Nil)))))) "size (Node ('c', 2, (Node  ('a', 1, Node ('b', 1, Nil)))))";;

(* Question 10: has_key tests *)
assert_equal (false) (has_key 'a' Nil) "has_key 'a' Nil";;
assert_equal (true) (has_key 'a' (Node ('a', 1, Node ('b', 2, Nil)))) "has_key 'a' (Node ('a', 1, Node ('b', 2, Nil)))";;
assert_equal (false) (has_key 'c' (Node ('a', 1, Node ('b', 2, Nil)))) "has_key 'c' (Node ('a', 1, Node ('b', 2, Nil)))";;

(* Question 11: key_values tests *)
assert_equal ([]) (key_values 'a' Nil) "key_values Nil";;
assert_equal ([1]) (key_values 'a' (Node ('a', 1, Node ('b', 2, Nil)))) "key_values 'a' (Node ('a', 1, Node ('b', 2, Nil)))";;
assert_equal ([2; 3]) (key_values 'a' (Node ('a', 2, Node ('a', 3, Nil)))) "key_values 'a' (Node ('a', 2, Node ('a', 3, Nil)))";;
assert_equal ([]) (key_values  'c' (Node ('a', 1, Node ('b', 2, Nil)))) "key_values 'c' (Node ('a', 1, Node ('b', 2, Nil)))";;

(* Question 12: combine tests *)
assert_equal (Node ('a', 1, Nil)) (combine (Node ('a', 1, Nil)) Nil) "combine (Node ('a', 1, Nil)) Nil";;
assert_equal ((Node ('b', 1, (Node ('a', 2, Node ('a', 3, Nil)))))) (combine (Node ('b', 1, Nil)) (Node ('a', 2, Node ('a', 3, Nil)))) "combine (Node ('b', 1, Nil)) (Node ('a', 2, Node ('a', 3, Nil)))";;
assert_equal ((Node ('a', 2, Node ('a', 3, Nil)))) (combine  Nil (Node ('a', 2, Node ('a', 3, Nil)))) "combine  Nil (Node ('a', 2, Node ('a', 3, Nil)))";;

(* Quest 13: group tests *)
assert_equal (Nil) (group Nil) "group Nil";;
assert_equal (Node ('a', [1], Nil)) (group (Node ('a', 1, Nil))) "group (Node ('a', 1, Nil))";;
assert_equal (Node ('a', [1], Node ('b', [2], Nil))) (group (Node ('a', 1, Node ('b', 2, Nil)))) "group (Node ('a', 1, Node ('b', 2, Nil)))";;
assert_equal (Node ('a', [1; 2], Nil)) (group (Node ('a', 1, Node ('a', 2, Nil)))) "group (Node ('a', 1, Node ('a', 2, Nil)))";;