let read_tuple value =
  let open Scanf in
  let f a b c d = (a, b), (c, d) in 
  sscanf value "%d-%d,%d-%d" f

let print_tuple tuple =
  let a = fst (fst tuple) in
  let b = snd (fst tuple) in
  let c = fst (snd tuple) in
  let d = snd (snd tuple) in
  Printf.printf "(%d,%d),(%d,%d)\n" a b c d

let rec read_lines () =
  match In_channel.input_line In_channel.stdin with
  | Some value -> [read_tuple value] @ read_lines ()
  | None -> []

let fully_contains tuple =
  let x1 = fst (fst tuple) in
  let y1 = snd (fst tuple) in
  let x2 = fst (snd tuple) in
  let y2 = snd (snd tuple) in
  let aux a b c d = a > d || b < c in
  if aux x1 y1 x2 y2 then
    0
  else
    1

let () =
  let items = read_lines () in 
  print_int (List.fold_left (fun acc tup -> acc + fully_contains tup) 0 items); 
  print_endline ""
