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
  let is_traversed (nd1 : 'a) (nd2 : 'a) : bool =
    if (nd1) == nd2 then true else false
  in
  let rec check (nd3: 'a) (lst: 'a mlist) : bool =
    match lst with
    | Nil -> false
    | Cons(c,n) ->
      if is_traversed nd3 c then true
      else check nd3 !n
  in
  let rec iter (lst: 'a mlist) : bool =
    match lst with
    | Nil -> false
    | Cons(c,n) -> 
      if (check c !n) then true
      else iter !n
  in iter lst
;;


(* Write a function flatten that flattens a list (removes its cycles
   if it has any) destructively. Again, you may want a recursive
   helper function and you shouldn't worry about space. *)
(* let flatten (lst : 'a mlist) : unit =
  if has_cycle lst then

  else () *)

(* Write mlength, which nondestructively finds the number of nodes in
   a mutable list that may have cycles. *)
let mlength (lst : 'a mlist) : int =
  failwith "mlength not implemented"

(* Please give us an honest estimate of how long this part took you to
   complete.  We care about your responses and will use them to help
   guide us in creating future assignments. *)
let minutes_spent : int = -1
