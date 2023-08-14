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
  let rec solve' str window =
  match str with
  | x :: xs ->
    if List.length window == 14 then
      if all_diff window then
        0
      else
        1 + solve' xs (List.tl window @ [x])
    else if all_diff window then
      1 + solve' xs (window @ [x])
    else
      1 + solve' xs (List.tl window @ [x]) 
  | _ -> 0
  in
  solve' str []

let list_of_string s =
  String.to_seq s |> List.of_seq

let () =
  let str = input_line stdin in 
  print_int (solve (list_of_string str))
