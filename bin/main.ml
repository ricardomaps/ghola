open Ghola
open Containers

let usage = "Usage: ghola [OPTION]"
let args = ref []
let anon arg = args := arg :: !args
let version = ref false
let recompile = ref false

let rec spec =
  [ ("--version", Arg.Set version, "Print the version number")
  ; ("--recompile", Arg.Set recompile, "Recompile your config")
  ]

let () =
  Arg.parse spec anon usage;
  Eio_main.run
  @@ fun env ->
  let dirs = Directories.get ~fs:env#fs in
  match () with
  | _ when !recompile ->
    let success = Compile.recompile ~force:true env#process_mgr dirs in
    if not success then exit 1
  | _ ->
    let success = Compile.recompile env#process_mgr dirs in
    if success then (
      let bin_name = Directories.bin_file dirs |> Eio.Path.native_exn in
      let args = Array.of_list (bin_name :: !args) in
      Unix.execv bin_name args
    ) else (
      let config_path = Eio.Path.native_exn dirs.config in
      prerr_endline @@ "Could not compile config at " ^ config_path ^ ", starting ghola with default config";
      Launch.run env Config.default
    )
;;
