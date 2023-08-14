type node = File of int64 | Folder of string

let solve () = 
  let open Hashtbl in 
  let graph = create 100000 in
  let parents = create 100000 in
  add graph "/" [];
  let rec read_lines current_dir =
    match In_channel.input_line In_channel.stdin with
    | None -> () 
    | Some line -> 
        let line' = String.trim line in

        print_endline line';

        let read_cd_cmd str = 
          Scanf.sscanf_opt str "$ cd %s" (fun x -> x)
        in

        let read_dir_cmd str = 
          Scanf.sscanf_opt str "dir %s" (fun x -> x)
        in

        let read_file_cmd str = 
          Scanf.sscanf_opt str "%Ld %s" (fun x _ -> x)
        in


        match read_cd_cmd line' with
        | Some dir ->
            if String.trim dir = ".." then
              let p = find parents current_dir in 
              read_lines p
            else if  String.trim dir = "/" then  
              read_lines "/" 
            else 
              let p = current_dir ^ dir ^ "/" in 
              read_lines p
        | None -> 
        match read_dir_cmd line' with
        | Some dir ->
            print_endline current_dir;
            let ndir = current_dir ^ dir ^ "/" in
            let old = find graph current_dir in
            add graph current_dir ((Folder ndir) :: old);
            add graph ndir [];
            add parents ndir current_dir;
            read_lines current_dir
        | None -> 
        match read_file_cmd line' with
        | Some sz ->
            let old = find graph current_dir in
            add graph current_dir ((File sz) :: old);
            read_lines current_dir
        | None -> read_lines current_dir
  in 

  let res = ref 0L in
  let rec sum x =
    match x with
    | File sz -> sz 
    | Folder f ->
        print_endline f;
        let children = find graph f in
        let vl = 
          List.fold_left (fun acc y ->
            Int64.add acc (sum y)) 0L children 
        in 
        if vl <= 100000L then
          res := Int64.add !res vl
        else
          res := !res;
        vl
  in 
 
  read_lines "/";
  sum (Folder "/");
  !res

let () =
  Printf.printf "%Ld\n" (solve ())
