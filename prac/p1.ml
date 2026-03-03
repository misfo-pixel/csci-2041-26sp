(*1.26*)

let rec length s = 
  match s with
  | [] -> 0
  | hd :: tl -> 1 + length(tl);;

(*1.28*)

(*immutability + persistence*)
let rec append l r =
  if r = []
  then l
  else
  match l with
  | [] -> r
  | hd :: tl -> hd :: (append tl r);;

(*internal helper function - Reduce stack overhead*)
let append l r =
  if r = []
  then l
  else let rec appending l =
    match l with
    | [] -> r
    | hd :: tl -> hd :: appending tl
  in appending l;;

(*lab1*)

(*1.30*)

(*normal recursion - n stack frame*)
let rec fac n =
  if n = 0
  then 1
  else n * fac(n-1);;

(*tail recursion - reuse 1 stack frame*)
let fac n = 
  let rec facing n r =
    if n = 0
    then r
    else facing (n-1) (n*r)
  in facing n 1;;

(*2.2*)

(*
double sqrt(double a)
{
double g = 1.0;
double h = a;
while (abs(g-h) ≥ µ)
  {
  g = (g+h)/2.0
  h = a/g;          <- new g!!!
  }
return g;
}
*)
let epsilon = 0.000001;;
let sqrt a =
  let rec sqrting g h =
    if abs_float(g -.h) < epsilon
    then g
    else
      let g' = (g +. h)/. 2.0
      in let h' = a /. g'
      in sqrting g' h'
  in sqrting 1.0 a;;

(*lab2*)

(*2.4*)

type intyChain = 
| Empty
| NotEmpty of int * intyChain;;

let e q =
  match q with
  | Empty -> 0
  | NotEmpty _ -> 1;;   (*a tuple behind NotEmpty*)

let ic = NotEmpty(1, NotEmpty(2, Empty));;

let length q = 
  let rec lengthing q r =
    match q with
    | Empty -> r
    | NotEmpty(_, q') -> lengthing q' r + 1
  in lengthing q 0;;

let sum q = 
  let rec summing q r =
    match q with
    | Empty -> r
    | NotEmpty(hd, tl) -> summing tl r + hd
  in summing q 0;;

(*parameterized type*)

type 'base chain = 
| EmptyChain
| NotEmptyChain of 'base * 'base chain;;

let nm = NotEmptyChain("Alice", NotEmptyChain("Benjamin", EmptyChain))
let id = NotEmptyChain(16, NotEmptyChain(14, EmptyChain))

let sum int q = 
  let rec summing q r =
    match q with
    | EmptyChain -> r
    | NotEmptyChain(hd, tl) -> summing tl r + hd
  in summing q 0;;

let isin element elements =
  let rec isinning elements =
    match elements with
    | EmptyChain -> false
    | NotEmptyChain(hd, tl) ->
      if hd = element
      then true
      else isinning tl
  in isinning elements;;

(*2.6*)

(*pattern matching*)

let g 5 = "five";;

(*This function can be called only with '()' -the unit objet*)
let euler () = "recieved";;

let hd (a::b) = a;;
let hd (a::_) = a;; (*better way to match*)

(*association list*)

(*Parametric Polymorphism*)
type ('key, 'value) al =
  ('key *'value) list;;
(*list form: [(k, v), (k, v)...]*)
let al myaldata = [(7, "CR");(10,"MS")];;

(*exception belongs to 'exn', an internal type*)
exception AlError of string
let alGet pairs key =
  let rec alGetting pairs =
    match pairs with
    | [] -> raise (AlError "No Such Key")
    | (k, v) :: tl ->
      if (k = key)
      then v
      else alGetting tl
  in alGetting pairs;;

let alPut key value pairs =
  let alPutting pairs =
    match pairs with
    | [] -> [(key, value)]
    | al -> (key, value) :: pairs
  in alPutting pairs;;

(*2.9*)

type 'key bst =
| BstEmpty
| BstNode of 'key * 'key bst * 'key bst;;

let bstIsIn key tree =
  let rec bstIsIning tree =
  match tree with
  | BstEmpty -> false
  | BstNode(k, left, right)->
    if k = key
    then true
    else if k > key
    then bstIsIning right
    else bstIsIning left
in bstIsIning tree;;

let bstInsert key tree = 
  let rec bstInserting tree = 
    match tree with
    | BstEmpty -> BstNode(key, BstEmpty, BstEmpty)
    | BstNode(k, l, r) -> 
      if key < k
      then BstNode(k, bstInserting l, r)
      else if key > k
      then BstNode(k, bstInserting r, l)
      else tree
  in bstInserting tree;;