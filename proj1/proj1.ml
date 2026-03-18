(* EXPRESSION. Represent an algebraic expression. *)

type expression =
  Var of string |                    (* a, b, c ... *)
  Neg of expression |                (*   ¬ R *)
  Add of expression * expression |   (* L + R *)
  Div of expression * expression |   (* L / R *)
  Equ of expression * expression |   (* L = R *)
  Mul of expression * expression |   (* L × R *)
  Sub of expression * expression ;;  (* L − R *)

(*
  Exception. 
  It’s raised when something erroneous or unexpected happens in the functions described below.
  It’s helpful if SolvingError contains a string that explains why it was raised.
*)
exception SolvingError of string

(* 
  A function of type string −> expression −> bool.
  Return true if a Var containing name appears at least once within expression.
  Return false otherwise.
*)
let rec isInside name expression =
  match expression with
  | Var e -> e = name
  | Neg e -> isInside name e
  | Add (l, r)
  | Div (l, r)
  | Equ (l, r)
  | Mul (l, r)
  | Sub (l, r) ->
    isInside name l || isInside name r;;

(*
  A function of type string −> expression −> expression −> expression.    
  This does all the work for the equation solver.

  Continue until finally a Var containing name is alone as left
  Then return Equ (left, right), representing the solved equation
*)
let rec solver name left right =
  if isInside name left then 
  match left with
  | Var v -> Equ (Var v, right)
  | Neg v -> 
      solver name v (Neg right)
  | Add (l, r) -> 
      if isInside name l
      then solver name l (Sub (right, r))
      else solver name r (Sub (right, l))
  | Div (l, r) -> 
      if isInside name l
      then solver name l (Mul (right, r))
      else solver name r (Div (l, right))
  | Mul (l, r) -> 
      if isInside name l
      then solver name l (Div (right, r))
      else solver name r (Div (right, l))
  | Sub (l, r) ->
      if isInside name l
      then solver name l (Add (right, r))
      else solver name r (Sub (l, right))
  | Equ (l, r) ->
      raise (SolvingError "Not a Equal")
  else if isInside name right then
    match right with
    | Var v -> Equ(left, Var v)
    | Neg v -> solver name (Neg left) v
    | Add (l, r) ->
        if isInside name r
        then solver name (Sub (left, l)) r
        else solver name (Sub (left, r)) l
    | Div (l, r) ->
        if isInside name r
        then solver name (Div (l, left)) r
        else solver name (Mul (r, left)) l
    | Mul (l, r) ->
        if isInside name r
        then solver name (Div (left, l)) r
        else solver name (Div (left, r)) l
    | Sub (l, r) ->
        if isInside name r
        then solver name (Sub (l, left)) r
        else solver name (Add (left, r)) l
    | Equ (l, r) ->
        raise (SolvingError "Unexpected Equ expression")
    else
      raise (SolvingError "No Vals")
    ;;

(* 
  A function of type string −> expression −> expression
  Here equation must have the form Equ (left, right)
*)
let solve name equation =
  match equation with
  | Equ (l, r) ->
    solver name l r
  | _ -> raise (SolvingError "Unexpected Equ expression")

let test1 =
  solve "x" (Equ (Add (Var "x", Var "b"), Var "v")) ;;

let test2 =
  solve "u" (Equ (Add (Mul (Var "a", Var "u"), Var "b"), Var "v")) ;;