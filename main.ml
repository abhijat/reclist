open Core_kernel

let make_csv words =
  let rec inner words =
    match words with
    | [] -> ""
    | [elem] -> elem
    | first :: rest -> first ^ "," ^ inner rest
  in
  "'" ^ (inner words) ^ "'"

let map_stuff () =
  let data = Map.of_alist_exn (module String)
               ["foo", 1; "bar", 2; "baz", 3] in
  let data = Map.add_exn data ~key:"abhijat" ~data:37 in
  print_endline (string_of_int (Map.find_exn data "abhijat"));
  print_endline (string_of_int (Map.length data))

let rec analyze_path path =
  let entries = Sys.readdir path in
  Array.iter ~f:(fun entry -> analyze_entry path entry) entries
and analyze_entry parent entry =
  let fullpath = Filename.concat parent entry in
  let isdir = Sys.is_directory fullpath in
  match isdir with
  | true ->
     begin
       printf "[D] %s\n" fullpath;
       try
         analyze_path fullpath
       with
         exn ->
         printf "ERROR: %s\n" (Exn.to_string exn)
     end
  | false ->
     printf "[F] %s\n" fullpath

  

let () =
  analyze_path "/tmp"
    
