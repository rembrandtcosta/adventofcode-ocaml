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
  let my_val = (Char.code (snd tuple) - 87) in
  let diff = my_val - (Char.code (fst tuple) - 64) in
  match diff with
  | -2 -> my_val + 6
  | -1 -> my_val + 0
  | 0 -> my_val + 3
  | 1 -> my_val + 6
  | _ -> my_val + 0

let () =
  let guide = read_lines () in
  print_endline (string_of_int (List.fold_left (+) 0 (List.map is_win guide)))
