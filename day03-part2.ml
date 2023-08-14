let rec read_lines () =
  let input = input_line stdin in
  match input with
  | "-1" -> []
  | _ -> [input] @ read_lines ()

let evaluate_char c =
  if Char.code c > Char.code 'Z' then
    Char.code c - Char.code 'a' + 1
  else
    Char.code c - Char.code 'A' + 27

let unique a b c =
  let condition1 x = String.contains b x in
  let condition2 x = String.contains c x in
  let conditions x = condition1 x && condition2 x in
  String.fold_left 
    (fun acc c ->
      if conditions c then
        acc ^ Char.escaped c
      else
        acc) "" a

let rec get_values items =
  match items with
  | a :: b :: c :: ts -> 
      evaluate_char (String.get (unique a b c) 0) + get_values ts
  | _ -> 0

let () =
  let items = read_lines () in
  print_endline (string_of_int (get_values items))
