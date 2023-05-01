(*
   Name: Ian Myers
   File: hw9.ml
   Date: Spring 2023
   Desc: 19 Functions in ocaml all doing various jobs
*)

(*Problem 1: function my_last that takes a list and returns the last element of the list*)
let rec my_last xs =
   match List.length xs with
   | 0 -> failwith "Empty List"
   | 1 -> List.hd xs
   | _ -> my_last (List.tl xs)
;;

(*Problem 2: function my_init that takes a list and returns a new list containing all of the elements of
the input list except for the last element*)
let rec my_init xs = 
   match List.length xs with
   | 0 -> failwith "Empty List"
   | 1 -> []
   | _ -> List.hd xs :: (my_init (List.tl xs))
;;

(*Problem 3: function my_replace that takes a pair of values and a list and returns a new list such that
each occurrence of the first value of the pair in the list is replaced with the second value*)
let rec my_replace (x1,x2) xs = 
   match xs with
   | [] -> []
   | x :: t when x = x1 -> x2 :: my_replace (x1,x2) t
   | x :: t -> x :: my_replace (x1, x2) t
;;

(*Problem 4: function my_replace_all that takes a list of pairs and a list of values and returns a
new list where each occurrence of the first value in a pair is replaced by the second value in the
pair*)
let rec my_replace_all xs ys =
   match (xs, ys) with
   | ([], ys') -> ys'
   | (xs', []) -> []
   | (x :: xs', _) -> my_replace_all xs' (my_replace (fst(x), snd(x)) ys)
;;

(*Problem 5:  function my_elem_sum that takes a value and a list, and returns the sum of the given values
in the list*)
let rec my_elem_sum x1 xs =
   match xs with
   | [] -> 0
   | x :: t when x = x1 -> x + my_elem_sum x1 t
   | x :: t -> my_elem_sum x1 t
;;

(*Problem 6: function my_range_sum v1 v2 that returns the sum of values starting at v1 and ending at v2*)
let rec my_range_sum v1 v2 = 
   match v1 with
   | _ when v1 > v2 -> 0
   | _ when v1 = v2 -> v2
   | _ -> v1 + my_range_sum (v1 + 1) v2
;;
 
type ('a, 'b) kvlist = Node of 'a * 'b * ('a, 'b) kvlist
| Nil ;;

let insert 'a 'b ('a, 'b) kvlist = Node of ()
