open Ghola
open Containers

let usage = "Usage: ghola [OPTION]"
let args = ref []
let anon arg = args := arg :: !args
let version = ref false
let recompile = ref false

let rec spec =
  [ "--version", Arg.Set version, "Print the version number"
  ; "--recompile", Arg.Set recompile, "Recompile your config"
  ]
;;

let () =
  Arg.parse spec anon usage;
  Eio_main.run
  @@ fun env ->
  let fs = env#fs in
  Directories.ensure_exists ~fs;
  match () with
  | _ when !recompile ->
    let success = Compile.recompile ~force:true env#process_mgr ~fs in
    if not success then exit 1
  | _ ->
    let success = Compile.recompile env#process_mgr ~fs in
    if success
    then
      let open Directories in
      let args = Array.of_list (bin_file_name :: !args) in
      Unix.execv bin_file_name args
    else (
      Printf.eprintf
        "Could not compile config at %s, starting ghola with default config"
        Directories.config_dir;
      Launch.run env Default.default)
;;
