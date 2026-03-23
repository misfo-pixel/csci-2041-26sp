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
  if n = 0 then []
  else
    try
      (lazyHead l) :: (lazyTake (lazyTail l) (n-1))
    with _ -> raise (LazyListError "Empty List")