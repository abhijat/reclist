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


let test_http () =
  let open Lwt in
  let open Cohttp in
  let open Cohttp_lwt_unix in
  let response = Client.get (Uri.of_string "https://www.reddit.com/r/askhistorians.json") in
  response >>=
    fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    printf "response code %d\n" code;
    printf "headers %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt.Body.to_string

let () =
  let body = Lwt_main.run (test_http ()) in
  print_endline body
