let makeStream this state next = 
  ((this, state), next) ;; 
 
let first ((this, state), next) = 
  this ;; 
 
let rest ((this, state), next) = 
  (next this state, next) ;;

let factorials = 
  makeStream 1 1 (fun this state -> (this * state, state + 1)) ;;

let rec take count stream = 
  match count 
  with 0 -> [] | 
       _ -> first stream :: take (count - 1) (rest stream) ;;

let odds =
  makeStream 1 2 (fun this state -> (this + state, state)) ;;

let rec trim count stream =
  match count
  with 0 -> stream |
       _ -> trim (count - 1) (rest stream) ;;

let scale factor stream =
  makeStream ((first stream) * factor) (rest stream) (fun this state -> (first state * factor, rest state)) ;;

let sum left right =
  makeStream (first left + first right) (rest left, rest right) (fun _ (r1, r2) -> ((first r1 + first r2), (rest r1, rest r2))) ;;
