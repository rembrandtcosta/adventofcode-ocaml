let remove_brackets str = 
  String.fold_left 
    (fun acc c ->
      if c == '\r' then
        acc
      else if c == '[' || c == ']' then
        acc ^ " " 
      else
        acc ^ Char.escaped c
    ) "" str

let rec read_lines () =
  match In_channel.input_line In_channel.stdin with
  | Some "\r" -> []
  | Some value -> (read_lines ()) @ [remove_brackets value]
  | None -> []

let parse_instruction str =
  let open Scanf in
  sscanf str "move %d from %d to %d" (fun a b c -> (a, b, c))

let rec read_instructions () =
  match In_channel.input_line In_channel.stdin with
  | Some value -> [parse_instruction value] @ (read_instructions ())
  | None -> []

let create_stacks str =
  let open Stack in
  let stk = create () in
  String.iter (fun c ->
    if c != ' ' then
      push c stk 
    else
      ()
  ) str;
  stk

let rec transpose lists =
  match lists with
  | [] -> []
  | [] :: xss -> transpose xss
  | (x :: xs) :: xss ->
      List.(
        (x :: map hd xss) :: transpose (xs :: map tl xss)
      )

let rec perfom_instructions instructions stacks =
  match instructions with
  | [] -> stacks
  | (h :: xs) ->
      match h with
      | (cnt, a, b) ->
          let mut = Array.of_list stacks in 
          let s1 = mut.(a-1) in
          let s2 = mut.(b-1) in
          let aux = Stack.create () in
          for i=1 to cnt do
            let elem = Stack.top s1 in
            Stack.push elem aux;
            Stack.pop s1;
          done;
          while Stack.is_empty aux = false do
            let elem = Stack.top aux in 
            Stack.push elem s2;
            Stack.pop aux;
          done;
          mut.(a-1) <- s1;
          mut.(b-1) <- s2;
          perfom_instructions xs (Array.to_list mut)

let () =
  let inp = read_lines () in 
  let inp' = List.map (fun x -> x |> String.to_seq |> List.of_seq) inp in
  let st = 
    List.map (fun x -> String.concat "" (List.map Char.escaped x)) (transpose inp') in
  let st' = List.fold_left 
    (fun acc s -> 
      if String.get s 0 != ' ' then
        acc @ [s] 
      else 
        acc
    ) [] st
  in
  let stacks = List.map create_stacks st' in 
  List.iter print_endline st';
  print_endline "";
  let ins = read_instructions () in
  let stacks' = perfom_instructions ins stacks in
  List.iter (fun s -> print_char (Stack.top s)) stacks';
  print_endline "";

