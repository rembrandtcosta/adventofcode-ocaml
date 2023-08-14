type node = Folders of string list | File of int64 

let empty_folder = Folders []

let insert_folder xs x =
  match xs with
  | File y -> []
  | Folders ys -> (x::ys)

let read_cd_command str =
  Scanf.sscanf_opt str "$ cd %s" (fun s -> s)

let read_dir str =
  Scanf.sscanf_opt str "dir %s" (fun s -> s)

let read_file str =
  Scanf.sscanf_opt str "%Ld %s" (fun x y -> (x, y))

let rec read_lines () = 
  let open Hashtbl in
  let graph = create 100000 in
  let parent = create 100000 in
  add graph "/" empty_folder;
  let rec exec current_dir =
    match In_channel.input_line In_channel.stdin with
    | None -> graph
    | Some line ->
        match read_cd_command line with
        | Some str -> 
            let go_to act_dir cmd =
              match cmd with
              | "/" -> "/"
              | ".." -> find parent act_dir 
              | dir -> (act_dir ^ dir ^ "/")
            in
            exec (go_to current_dir str) 
        | None -> 
        match read_dir line with
        | Some dir ->
            let new_dir = current_dir ^ dir ^ "/" in
            add graph new_dir empty_folder;
            let old = find graph current_dir in
            let nw = insert_folder old (new_dir) in
            add graph current_dir (Folders(nw));
            add parent new_dir current_dir;
            exec current_dir;
        | None ->
        match read_file line with
        | Some (filesize, name) ->
            add graph name (File filesize);
            let old = Hashtbl.find graph current_dir in 
            let nw = insert_folder old name in 
            add graph current_dir (Folders(nw));
            exec current_dir;
        | None -> exec current_dir;
  in 
  exec "/"

let pretty_print graph = 
  let rec pretty_print' x = 
    match x with
    | File sz ->
        print_endline ""
    | Folders fs ->
        List.iter print_endline fs;
        print_endline "";
        List.iter (fun f -> pretty_print' (Hashtbl.find graph f)) fs;
  in
  pretty_print' (Hashtbl.find graph "/")

let sum graph =
  let res = ref 0L in
  let rec sum' x =
    match x with
    | File sz -> 
        sz
    | Folders fs ->
        let vl = List.fold_left (fun acc y -> Int64.add acc (sum' (Hashtbl.find graph y))) 0L fs in
        if vl <= 100000L then
          res := Int64.add !res vl
        else 
          res := !res;
        vl
  in 
  sum' (Hashtbl.find graph "/");
  !res

let () =
  let graph = read_lines () in
  pretty_print graph;
  print_endline "";
  Printf.printf "%Ld" (sum graph);
  print_endline ""

