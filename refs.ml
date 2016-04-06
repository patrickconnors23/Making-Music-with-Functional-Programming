(**********************************************************************
			  CS51 Problem Set 6
			     Spring 2016
		       Refs, Streams, and Music

			     Part 1: Refs
 **********************************************************************)

(* The type of mutable lists. *)
type 'a mlist = Nil | Cons of 'a * 'a mlist ref

(* Write a function has_cycle that returns whether a mutable list has
   a cycle.  You may want a recursive helper function. Don't worry
   about space usage. *)
let has_cycle (lst : 'a mlist) : bool =
  let is_traversed (nd1 : 'a) (nd2 : 'a list) : bool =
    if List.memq nd1 nd2 then true else false
  in
  let rec iter (lst: 'a mlist) (lst_build : 'a list) : bool =
    match lst with
    | Nil -> false
    | Cons(c,n) -> 
      if is_traversed c lst_build then true
      else iter !n (lst_build@[c])
  in iter lst []
;;


(* Write a function flatten that flattens a list (removes its cycles
   if it has any) destructively. Again, you may want a recursive
   helper function and you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let is_traversed (nd1 : 'a) (nd2 : 'a list) : bool =
    if List.memq nd1 nd2 then true else false
  in
  let rec iter (lst: 'a mlist) (lst_build : 'a list) : unit =
    match lst with
    | Nil -> ()
    | Cons(c,n) -> 
      if is_traversed c lst_build then n:=Nil
      else iter !n (lst_build@[c])
  in iter lst []
;;

(* Write mlength, which nondestructively finds the number of nodes in
   a mutable list that may have cycles. *)
let mlength (lst : 'a mlist) : int =
  let is_traversed (nd1 : 'a) (nd2 : 'a list) : bool =
    if List.memq nd1 nd2 then true else false
  in
  let rec len (lst: 'a mlist) (lst_build:'a list) (counter: int) : int =
    match lst with
    | Nil -> counter
    | Cons(c,n) -> 
      if is_traversed c lst_build then 
        let counter1 = counter + 1 in counter1
      else len (!n) (lst_build@[c]) (counter+1)
  in len lst [] 0
;;

(* Please give us an honest estimate of how long this part took you to
   complete.  We care about your responses and will use them to help
   guide us in creating future assignments. *)
let minutes_spent : int = 30
