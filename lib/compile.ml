open! Containers

let ( / ) = Eio.Path.( / )

type compile_method =
  | NixFlake
  | NixDefault
  | Dune
  | Ocamlopt

let name = "ghola"

let detect_compile Directories.{ config = dir; _ } =
  let open Eio.Path in
  match () with
  | _ when is_file (dir / "flake.nix") ->
    print_endline
    @@ "Ghola will use nix flake at "
    ^ native_exn (dir / "flake.nix")
    ^ " to recompile";
    NixFlake
  | _ when is_file (dir / "default.nix") ->
    print_endline
    @@ "Ghola will use nix file at "
    ^ native_exn (dir / "default.nix")
    ^ " to recompile";
    NixDefault
  | _ when is_file (dir / "dune-project") ->
    print_endline @@ "Ghola will use dune build at " ^ native_exn dir ^ " to recompile";
    Dune
  | _ ->
    print_endline @@ "Ghola will use ocamlopt at " ^ native_exn dir ^ " to recompile";
    Ocamlopt
;;

let report_errors status dirs =
  let err_file = Directories.err_file dirs in
  let errs = Eio.Path.load err_file in
  prerr_endline "Errors detected while compiling Ghola config";
  prerr_endline @@ "Exit code: " ^ Int.to_string status;
  if not @@ String.is_empty errs then prerr_string errs
;;

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

(* the -I flag in ocamlopt doesn't recurse so it's not necessary to recurse here *)
let sources dirs =
  let lib = Directories.lib_file dirs in
  let src = Directories.src_file dirs in
  let libs =
    try
      Eio.Path.read_dir lib
      |> List.filter (String.suffix ~suf:".ml")
      |> List.map (fun f -> lib / f)
    with _ -> []
  in
  src :: libs
;;

let should_compile dirs = function
  | Ocamlopt ->
    let bin = Directories.bin_file dirs in
    (match mtime bin with
    | None -> true
    | Some time ->
      List.filter_map mtime (sources dirs)
      |> List.exists (Float.(<) time))
  | _ -> true

let compile mgr dirs meth =
  let open Result in
  let ( >> ) r1 r2 = r1 >>= fun () -> r2 in
  let err_file = Directories.err_file dirs in
  let bin_file = Directories.bin_file dirs in
  let copy_bin file =
    Eio.Path.(load file |> save ~create:(`Or_truncate 0o755) bin_file)
  in
  let copy_result dir =
    Eio.Path.read_dir dir
    |> List.sort String.compare
    |> List.find_opt (fun file -> is_executable (dir / file))
    |> function
    | None -> Error 1
    | Some path -> Ok (copy_bin @@ (dir / path))
  in
  let run cmd =
    let open Eio.Process in
    Eio.Path.with_open_out ~create:(`Or_truncate 0o644) err_file
    @@ fun stderr ->
    Eio.Switch.run
    @@ fun sw ->
    match await @@ spawn mgr ~sw ~cwd:dirs.config ~stderr cmd with
    | `Exited 0 -> Ok ()
    | `Exited code -> Error code
    | `Signaled signal -> Error 1
    | exception _ -> Error 1
  in
  match meth with
  | NixFlake -> run [ "nix"; "build" ] >> copy_result (dirs.config / "result/bin")
  | NixDefault -> run [ "nix-build" ] >> copy_result (dirs.config / "result/bin")
  | Dune ->
    run [ "dune"; "build" ]
    >> return @@ copy_bin (dirs.config / "_build/default/bin/main.exe")
  | Ocamlopt ->
    run
      [ "ocamlfind"
      ; "ocamlopt"
      ; "-package"
      ; "ghola"
      ; "-linkpkg"
      ; "-I"
      ; "lib"
      ; "ghola.ml"
      ; "-o"
      ; Eio.Path.native_exn bin_file
      ]
;;

let recompile ?(force = false) mgr dirs =
  let meth = detect_compile dirs in
  if force || should_compile dirs meth then
    (match compile mgr dirs meth with
    | Error code -> report_errors code dirs; false
    | Ok () -> true)
  else
    true
