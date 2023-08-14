let rec read_lines () = 
  match In_channel.input_line In_channel.stdin with
  | Some line -> String.trim line :: (read_lines ())
  | None -> []

let array_of_string s = 
  String.to_seq s |> Array.of_seq

let solve trees = 
  let vis = Array.init (Array.length trees)
    (fun _ -> Array.init (Array.length trees.(0)) (fun _ -> 1)) in
  let lr = 
    let n = Array.length trees in
    for i = 0 to n-1 do 
      let mx = Array.init 11 (fun _ -> 0) in
      let m = Array.length trees.(i) in 
      for j = 0 to m-1 do 
        let pos = ref 0 in
        let x = Char.code trees.(i).(j) - Char.code '0' in 
        for k = x to 9 do 
          pos := max !pos (mx.(k));
        done;
        vis.(i).(j) <- vis.(i).(j) * (j - !pos);
        mx.(x) <- j;
      done;
      vis.(i).(0) <- 0;
      vis.(i).(m-1) <- 0;
    done;
  in 
  let rl = 
    let n = Array.length trees in
    for i = n-1 downto 0 do 
      let m = Array.length trees.(i) in 
      let mx = Array.init 11 (fun _ -> m-1) in
      for j = m-1 downto 0 do 
        let pos = ref m in
        let x = Char.code trees.(i).(j) - Char.code '0' in 
        for k = x to 9 do 
          pos := min !pos (mx.(k));
        done;
        vis.(i).(j) <- vis.(i).(j) * (!pos - j);
        mx.(x) <- j;
      done;
      vis.(i).(0) <- 0;
      vis.(i).(m-1) <- 0;
    done;
  in 
  let tb = 
    let n = Array.length trees in
    let m = Array.length trees.(0) in 
    for j = 0 to m-1 do 
      let mx = Array.init 11 (fun _ -> 0) in
      for i = 0 to n-1 do 
        let pos = ref 0 in
        let x = Char.code trees.(i).(j) - Char.code '0' in 
        for k = x to 9 do 
          pos := max !pos (mx.(k));
        done;
        vis.(i).(j) <- vis.(i).(j) * (i - !pos);
        mx.(x) <- i;
      done;
      vis.(0).(j) <- 0;
      vis.(n-1).(j) <- 0;
    done;
  in
  let bt = 
    let n = Array.length trees in
    let m = Array.length trees.(0) in 
    for j = m-1 downto 0 do 
      let mx = Array.init 11 (fun _ -> m-1) in
      for i = n-1 downto 0 do 
        let pos = ref n in
        let x = Char.code trees.(i).(j) - Char.code '0' in 
        for k = x to 9 do 
          pos := min !pos (mx.(k));
        done;
        vis.(i).(j) <- vis.(i).(j) * (!pos - i);
        mx.(x) <- i;
      done;
      vis.(0).(j) <- 0;
      vis.(n-1).(j) <- 0;
    done;
  in 
  
  lr;
  rl;
  tb;
  bt;

  let res = ref 0 in 
  print_endline "";

  for i = 0 to Array.length trees - 1 do 
    for j = 0 to Array.length trees.(i) - 1 do 
      res := max !res vis.(i).(j);
    done 
  done;

  !res


let () = 
  let items = read_lines () in 
  let mat = Array.of_list (List.map array_of_string items) in
  print_int (solve mat);
  print_endline "";


