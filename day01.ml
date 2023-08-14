let insert_top3 ls x =
  List.tl (List.sort compare (ls @ [x]))

let rec read_lines index value top3 = 
  let input = read_line () in
  let x = int_of_string_opt input in
  match x with 
  | Some res -> 
      let nval = (res + value) in
      if value == -1 then
        List.fold_left (+) 0 top3  
      else
        read_lines index nval top3
  | None -> 
      let ntop3 = insert_top3 top3 value in
      max value (read_lines (index+1) 0 ntop3)

let () =
  print_endline (string_of_int (read_lines 1 0 [0; 0; 0]))
