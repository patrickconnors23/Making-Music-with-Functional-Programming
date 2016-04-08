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
  let rec iter (lst: 'a mlist) (lst_build : 'a list) : bool =
    match lst with
    (* if the end of the list is reached then it can't have cycle *)
    | Nil -> false
    | Cons(c,n) -> 
      (* check whether it points to anything that has been traversd *)
      if List.memq c lst_build then true
      (* if not then add it to comp and try again on tail *)
      else iter !n (lst_build@[c])
  in iter lst []
;;


(* Write a function flatten that flattens a list (removes its cycles
   if it has any) destructively. Again, you may want a recursive
   helper function and you shouldn't worry about space. *)
let flatten (lst : 'a mlist) : unit =
  let rec iter (lst: 'a mlist) (lst_comp: 'a mlist) : unit =
    match lst,lst_comp with
    (* if one is Nil then there can't be anything to flatten *)
    | Nil, _ -> ()
    | _ ,Nil -> ()
    (* otherwise we have to match again *)
    | Cons(h1,t1), Cons(h2,t2) -> 
      match !t2 with
      (* if tail is nil then nothing to flatten *)
      | Nil -> ()
      | Cons(h3,t3) -> 
        (* if tail points to part of list then make it Nil *)
        if !t1 == !t3 then t1 := Nil
        (* otherwise call again with tail and tail of tail *)
        else iter !t1 !t3
  in iter lst lst
;;

(* Write mlength, which nondestructively finds the number of nodes in
   a mutable list that may have cycles. *)
let mlength (lst : 'a mlist) : int =
  let rec len (lst: 'a mlist) (lst_build:'a list) (counter: int) : int =
    match lst with
    (* end of list return counter *)
    | Nil -> counter
    | Cons(c,n) -> 
      (* if there's a cycle then return counter + 1 *)
      if List.memq c lst_build then 
        let counter1 = counter + 1 in counter1
      (* otherwise increment counter and call again keeping track of whats
          been traversed
       *)
      else len (!n) (lst_build@[c]) (counter+1)
  in len lst [] 0
;;

(* Please give us an honest estimate of how long this part took you to
   complete.  We care about your responses and will use them to help
   guide us in creating future assignments. *)
let minutes_spent : int = 75
