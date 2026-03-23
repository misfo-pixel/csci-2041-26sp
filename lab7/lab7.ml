open Lazy;;

type 'a lazyList =
  | LazyEmpty
  | LazyNode of 'a Lazy.t * ('a lazyList) Lazy.t

exception LazyListError of string

let lazyCons h t = LazyNode (h, t);;

let lazyHead l =
  match l with
  | LazyEmpty -> raise (LazyListError "Empty List")
  | LazyNode (h, _) -> force h ;;

let lazyTail l =
  match l with
  | LazyEmpty -> raise (LazyListError "Empty List")
  | LazyNode (_, t) -> force t ;;

let rec lazyTake l n =
  match l with
  | LazyEmpty -> raise (LazyListError "Empty List")
  | LazyNode (h, t) ->
    force h ;
    lazyTake (force t) (n-1) ;;

let lazyFibs () = 
  let rec lazyFibbing left right = 
    lazyCons 
      (lazy (Printf.printf "Computed Fibonacci %i\n" left ; left)) 
      (lazy (lazyFibbing right (left + right))) 
  in lazyFibbing 0 1 ;;
