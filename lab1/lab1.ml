let howMany e l =
  let rec howMany l s =
    match l with
    | [] -> s
    | h :: t -> 
        if h = e then howMany t s+1
        else howMany t s
  in howMany l 0;;

let rec delete e l =
  match l with
  | [] -> []
  | h :: t ->
      if h = e then delete e t
      else h :: (delete e t)

let mean l =
  let rec mean l sum size=
    match l with
      | [] -> sum/.size
      | h :: t ->
          mean t (sum +. h) (size +. 1.0)
      in mean l 0.0 0.0;;