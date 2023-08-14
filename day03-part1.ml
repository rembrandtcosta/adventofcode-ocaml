let items_in_both str = 
  let hlf = (String.length str / 2) in
  let fst = String.sub str 0 hlf in
  let snd = String.sub str hlf hlf in

  String.fold_left 
    (fun acc c -> 
      if (String.contains snd c && not (List.exists (fun x -> x==c) acc)) then 
        c :: acc 
      else 
        acc) [] fst

let value_of str =
  let is_in_both = items_in_both str in
  let evaluate_chars chr =
    if Char.code chr > Char.code 'Z' then
      Char.code chr - Char.code 'a' + 1
    else
      Char.code chr - Char.code 'A' + 27
  in
  List.fold_left (fun acc c -> acc + evaluate_chars c) 0 is_in_both


let rec read_lines () =
  let input = input_line stdin in
  match input with
  | "-1" -> []
  | _ -> [value_of input] @ (read_lines ())

let () =
  let items = read_lines () in
  print_endline (string_of_int (List.fold_left (+) 0 items))
