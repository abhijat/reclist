open Core_kernel

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
    
