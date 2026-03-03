let rec allbut things thing=
  match things with
  | [] -> []
  | h :: t ->
    if h = thing then t
    else h :: allbut t thing;;

let rec choose etc things=
  match things with
  | [] -> ()
  | h :: t -> 
    etc h;
    choose etc t;;

let permute etc things=
  let rec permuting etc p u =
    match u with
    | [] -> etc p
    | _ ->
      choose (fun x -> 
        permuting etc (x :: p) (allbut u x)
        ) u
  in permuting etc [] things
