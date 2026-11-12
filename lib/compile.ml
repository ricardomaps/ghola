(* this whole thing is absolute garbage *)
open! Containers

let ( / ) = Eio.Path.( / )

type compile_method =
  | NixFlake
  | NixDefault
  | Dune
  | Ocamlopt
  | Script of string

let is_executable path =
  match Eio.Path.stat ~follow:true path with
  | { kind = `Regular_file; perm; _ } -> perm land 0o111 <> 0
  | _ -> false
  | exception _ -> false
;;

let mtime path =
  match Eio.Path.stat ~follow:true path with
  | st -> Some st.Eio.File.Stat.mtime
  | exception _ -> None
;;

let copy src ~to_ =
  let tmp =
    match Eio.Path.split to_ with
    | Some (dir, leaf) -> dir / (leaf ^ ".tmp")
    | None -> invalid_arg "copy: destination has no parent directory"
  in
  let cleanup () =
    try Eio.Path.unlink tmp with
    | _ -> ()
  in
  try
    cleanup ();
    Eio.Path.(load src |> save ~create:(`Exclusive 0o755) tmp);
    Eio.Path.rename tmp to_;
    Ok ()
  with
  | Eio.Io _ as e ->
    cleanup ();
    Error
      (Fmt.str "Failed to copy %a to %a: %a" Eio.Path.pp src Eio.Path.pp to_ Eio.Exn.pp e)
;;

let detect_compile Directories.{ config = dir; _ } =
  let open Eio.Path in
  let try_script () =
    let script_file = dir / "build" in
    if is_file script_file
    then
      if is_executable script_file
      then (
        print_endline
        @@ "Ghola will use build script at "
        ^ native_exn script_file
        ^ " to recompile.";
        Some (Script (native_exn script_file)))
      else (
        print_endline
        @@ "Ghola will not use build script, because "
        ^ native_exn script_file
        ^ " is not executable";
        None)
    else None
  in
  let try_nix_flake () =
    let flake_file = dir / "flake.nix" in
    if is_file flake_file
    then (
      print_endline
      @@ "Ghola will use nix flake at "
      ^ native_exn flake_file
      ^ " to recompile";
      Some NixFlake)
    else None
  in
  let try_nix_default () =
    let nix_file = dir / "default.nix" in
    if is_file nix_file
    then (
      print_endline
      @@ "Ghola will use nix file at "
      ^ native_exn nix_file
      ^ " to recompile";
      Some NixDefault)
    else None
  in
  let try_dune () =
    let dune_file = dir / "dune-project" in
    if is_file dune_file
    then (
      print_endline @@ "Ghola will use dune build at " ^ native_exn dir ^ " to recompile";
      Some Dune)
    else None
  in
  let use_ocamlopt () =
    print_endline @@ "Ghola will use ocamlopt at " ^ native_exn dir ^ " to recompile";
    Ocamlopt
  in
  match
    List.find_map (fun f -> f ()) [ try_script; try_nix_flake; try_nix_default; try_dune ]
  with
  | Some meth -> meth
  | None -> use_ocamlopt ()
;;

let report_errors dirs =
  try
    let err_file = Directories.err_file dirs in
    let errs = Eio.Path.load err_file in
    if not @@ String.is_empty errs
    then (
      prerr_endline "Errors detected while compiling Ghola config";
      prerr_string errs;
      prerr_string ("Check " ^ Eio.Path.native_exn err_file))
  with
  | _ -> ()
;;

let should_compile dirs = function
  | Ocamlopt ->
    let btime = Directories.bin_file dirs |> mtime in
    let stime = Directories.src_file dirs |> mtime in
    (match btime, stime with
     | Some btime, Some stime -> Float.(btime < stime)
     | _ -> true)
  | _ -> true
;;

let compile mgr dirs meth =
  let open Result in
  let cwd = Directories.(dirs.config) in
  let bin_file = Directories.bin_file dirs in
  let src_file = Directories.src_file dirs in
  let err_file = Directories.err_file dirs in
  let copy_bin file =
    match copy file ~to_:bin_file with
    | Ok () -> Ok ()
    | Error msg ->
      Eio.Path.save ~append:true ~create:(`If_missing 0o644) err_file (msg ^ "\n");
      Error 1
  in
  let run cmd =
    let open Eio.Process in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) err_file
    @@ fun stderr ->
    let fail code msg =
      Eio.Flow.copy_string (msg ^ "\n") stderr;
      Error code
    in
    Eio.Switch.run
    @@ fun sw ->
    match await @@ spawn mgr ~sw ~cwd ~stderr cmd with
    | `Exited 0 -> Ok ()
    | `Exited code -> fail code @@ Fmt.str "Process failed with exit code: %d" code
    | `Signaled _ as status ->
      fail 1 @@ Fmt.str "Process died: %a" Eio.Process.pp_status status
    | exception e ->
      fail 1 @@ Fmt.str "Failed to run %a: %a" Eio.Process.pp_args cmd Eio.Exn.pp e
  in
  match meth with
  | Script path -> run [ path; Eio.Path.native_exn bin_file ]
  | NixFlake -> run [ "nix"; "build" ] >>= fun () -> copy_bin (cwd / "result/bin/config")
  | NixDefault -> run [ "nix-build" ] >>= fun () -> copy_bin (cwd / "result/bin/config")
  | Dune ->
    run [ "dune"; "build"; "bin/config.exe" ]
    >>= fun () -> copy_bin (cwd / "_build/default/bin/config.exe")
  | Ocamlopt ->
    run
      [ "ocamlfind"
      ; "ocamlopt"
      ; "-package"
      ; "ghola"
      ; "-linkpkg"
      ; Eio.Path.native_exn src_file
      ; "-o"
      ; "config.exe"
      ]
    >>= fun () -> copy_bin (cwd / "config.exe")
;;

let recompile ?(force = false) mgr dirs =
  let meth = detect_compile dirs in
  if force || should_compile dirs meth
  then (
    match compile mgr dirs meth with
    | Error _ ->
      report_errors dirs;
      false
    | Ok () -> true)
  else true
;;
