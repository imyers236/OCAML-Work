(*
   Name: Ian Myers
   File: hw9.ml
   Date: Spring 2023
   Desc: 17 Functions in ocaml all doing various jobs
*)

(*Problem 1: function my_last that takes a list and returns the last element of the list*)
let rec my_last xs =
   match xs with
   | [] -> failwith "Empty List"
   | [x] -> x
   | _ :: t -> my_last t
;;

(*Problem 2: function my_init that takes a list and returns a new list containing all of the elements of
the input list except for the last element*)
let rec my_init xs = 
   match xs with
   | [] -> failwith "Empty List"
   | [x] -> []
   | x :: t -> x :: (my_init t)
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

(*Problem 7: function insert with type α → β → (α, β) kvlist → (α, β) kvlist*)
let insert k v l = Node (k, v, l);;

(*Problem 8: function remove with type α → (α, β) kvlist → (α, β) kvlist that removes all key-value
pairs in a collection that have a given key*)
let rec remove k l = 
   match l with
   | Nil -> Nil
   | Node (a, b, c) when a = k -> remove k c
   | Node (a, b, c) -> Node (a, b, (remove k c))
;;

(*Problem 9: function size with type (α, β) kvlist → int. The size function should return the number
of key-value pairs in the collection, where size Nil is 0*)
let rec size l =
   match l with 
   | Nil -> 0
   | Node (a, b, t) -> 1 + size t
;;

(*Problem 10: function has_key with type α → (α, β) kvlist → bool, which returns true if the collection
contains a key-value pair with the given key, and false otherwise*)
let rec has_key k l =
   match l with
   | Nil -> false
   | Node (a, b, c) when k = a -> true
   | Node (a, b, c) -> has_key k c
;;


(*Problem 11: function key_values with type α → (α, β) kvlist → β list. This function should return
a list of the values for a given key in a collection*)
let rec key_values k l = 
   match l with
   | Nil -> []
   | Node (a, b, c) when k = a -> [b] @ (key_values k c) 
   | Node (a, b, c) -> (key_values k c)
;;

(*Problem 12: function combine with type (α, β) kvlist → (α, β) kvlist → (α, β) kvlist. This function should work the same as (@) but for key-value collections*)
let rec combine x y = 
   match x with
   | Nil -> y
   | Node (a, b, c) -> Node (a, b, (combine c y))
;;

(*Problem 13: function group with type (α, β) kvlist → (α, β list) kvlist. This function should
combine key-value pairs with duplicate keys*)
let rec group l =
   match l with
   | Nil -> Nil
   | Node (a, b, c) -> Node (a, (key_values a l), group (remove a c))
;;

(*Problem 14: function invert with type (α, β) kvlist → (β, α) kvlist. This function simply “flips”
each key-value pair in the collection*)
let rec invert l =
   match l with
   | Nil -> Nil
   | Node (a, b, c) -> Node (b, a, invert c)
;;

(*Problem 15: function kv_filter with type (α → β → bool) → (α, β) kvlist → (α, β) kvlist. This
function should be identical to the filter function but work over kvlist values as opposed to lists*)
let rec kv_filter f l =
   match l with
   | Nil -> Nil
   | Node (a, b, c) when f a b -> Node (a, b, kv_filter f c)
   | Node (a, b, c) -> kv_filter f c
;;

(*Checks whether the inputted b is less than ten returns true if b < 10*)
let rec less_ten a b = 
   match b with
   | _ when b < 10 -> true
   | _ -> false
;;

(*Problem 16: function kv_map with type (α → β → γ ∗ δ) → (α, β) kvlist → (γ, δ) kvlist. This
function should be identical to the map function but work over kvlist values as opposed to lists.*)
let rec kv_map f l =
   match l with
   | Nil -> Nil
   | Node (a, b, c) -> Node (fst(f a b), snd(f a b), kv_map f c)
;;

(*Adds ten to b, returns tuples of a and new b*)
let rec add_ten a b = (a, (b + 10));;

(*Problem 17: function count_keys_by_val with type int → (α, β) kvlist → (β, int) kvlist. The first parameter is a “threshold” value. 
The function returns the number of key-value pairs each value is associated such that the number of key-value pairs is larger than the threshold value*)

let rec count_keys_by_val n l = let w = group (invert l) in let y = kv_filter (fun a b -> List.length(b) > n) w in kv_map (fun a b -> (a, List.length(b))) y;; 
 





