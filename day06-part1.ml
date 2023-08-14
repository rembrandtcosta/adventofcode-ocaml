let all_diff xs =
  let xss = List.sort compare (List.map Char.code xs) in 
  let rec eq yss =
    match yss with
    | a :: b :: [] ->
        not (a == b)
    | a :: b :: ys -> 
        if a == b then
          false
        else
          eq (b :: ys) 
    | _ -> true
  in 
  eq xss
  
let solve str =
  let rec solve' cnt str =
  match str with
  | a :: b :: c :: d :: xs ->
      if all_diff [a; b; c; d] then
        cnt
      else 
        solve' (cnt+1) (b :: c :: d :: xs)
  | a :: b :: c :: xs -> 
      if all_diff [a; b; c] then
        cnt
      else
        solve' (cnt+1) (b :: c :: xs)
  | a :: b :: xs -> 
      if all_diff [a; b] then
        cnt
      else 
        solve' (cnt+1) (b :: xs)
      | _ -> cnt
  in 
  solve' 0 str

let list_of_string s =
  String.to_seq s |> List.of_seq

let () =
  let str = input_line stdin in 
  print_int (solve (list_of_string str) + 4)
