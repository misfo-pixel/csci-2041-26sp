let rec c n k =
  match k with
  | 0 -> 1
  | _ -> if n = 0 then 0 else c (n-1) k + c (n-1) (k-1)

let memyC n k =
  let memo = Hashtbl.create 100 in
  let rec c n k =
    match Hashtbl.find_opt memo (n, k) with
    | Some v -> v
    | None ->
        let v =
          match k with
          | 0 -> 1
          | _ -> 
              if n = 0 then 0
              else c (n-1) k + c (n-1) (k-1)
        in
        Hashtbl.add memo (n, k) v;
        v
  in
  c n k