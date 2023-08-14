let string_to_tuple str =
  let lst = String.split_on_char ' ' str in
  let to_char = List.map (fun x -> String.get x 0) lst in
  (List.nth to_char 0, List.nth to_char 1)

let rec read_lines () = 
  let input = input_line stdin in
  match input with
  | "-1" -> []
  | _ -> [string_to_tuple input] @ (read_lines ())

let print_tuple tuple = 
  Printf.printf "(%c, %c)" (fst tuple) (snd tuple)

let is_win tuple = 
  let need = (Char.code (snd tuple) - 87) in
  let his = (Char.code (fst tuple) - 64) in
  match need with
  | 1 -> (*lose*)
      (3 + his + 1) mod 3 + 1
  | 2 -> (*draw*)
      his + 3
  | 3 -> (*win*)
      (3 + his) mod 3 + 1 + 6

let () =
  let guide = read_lines () in
  print_endline (string_of_int (List.fold_left (+) 0 (List.map is_win guide)))
