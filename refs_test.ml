(**********************************************************************
			  CS51 Problem Set 6
			     Spring 2016
		       Refs, Streams, and Music

			 Part 1: Refs Testing
 **********************************************************************)

(* Make your refs solution available for testing *)
open Refs ;;

(* Establish some mutable lists for testing. *)
let list1a = Cons(2, ref Nil) ;;
let list1b = Cons(2, ref list1a) ;;
let list1 = Cons(1, ref list1b) ;;

let reflist = ref (Cons(2, ref Nil)) ;;
let list2 = Cons(1, ref (Cons (2, reflist))) ;;
let _ = reflist := list2 ;;

(* Some example tests. You'll want more. *)
let test_has_cycle = 
	assert(not(has_cycle list1a));
	assert(has_cycle(!reflist));;

let test_flatten = 
	assert(flatten list1a; list1a = Cons(2,{contents = Nil}));
	assert(flatten list2; list2 
		= Cons(1,{contents = Cons(2,{contents = Nil})}));;

let test_mlength =
	assert(mlength list1a = 1);
	assert(mlength list1b = 2);
	assert(mlength list1 = 3);
	assert(mlength list2 = 2);;

test_has_cycle;;
test_flatten;;
test_mlength;;
