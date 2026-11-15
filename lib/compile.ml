open! Containers

let ( / ) = Eio.Path.( / )

type compile_method =
  | NixFlake
  | NixDefault
  | Dune
  | Ocamlopt
  | Script

let detect_compile dir =
  List.find_map
    (fun (f, meth) -> Option.return_if (Eio.Path.is_file @@ (dir / f)) meth)
    [ "build", Script
    ; "flake.nix", NixFlake
    ; "default.nix", NixDefault
    ; "dune-project", Dune
    ; "config.ml", Ocamlopt
    ]
;;

let compile mgr (Directories.{ config; cache; _ } as dirs) meth =
  let open Directories in
  let ( >>= ) = Result.( >>= ) in
  let config_dir = Eio.Path.native_exn config in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (err_file dirs)
  @@ fun stderr ->
  let fail msg =
    Eio.Flow.copy_string (msg ^ "\n") stderr;
    Error msg
  in
  let install ?(cwd = cache) src =
    let tmp = cache / "ghola-config.tmp" in
    try
      Eio.Path.unlink ~missing_ok:true tmp;
      Eio.Path.(load (cwd / src) |> save ~create:(`Exclusive 0o755) tmp);
      Eio.Path.rename tmp (bin_file dirs);
      Ok ()
    with
    | Eio.Io _ as e ->
      fail
      @@ Fmt.str
           "Failed to copy %a to %a: %a"
           Eio.Path.pp
           (cwd / src)
           Eio.Path.pp
           (bin_file dirs)
           Eio.Exn.pp
           e
  in
  let run ?(cwd = cache) cmd =
    try
      Eio.Process.run mgr ~cwd ~stderr ~stdout:stderr cmd;
      Ok ()
    with
    | Eio.Exn.Io (e, _) -> fail @@ Fmt.str "%a" Eio.Exn.pp_err e
  in
  match meth with
  | Script -> run [ config_dir ^ "/build"; bin_file_name dirs ]
  | NixFlake ->
    run [ "nix"; "build"; config_dir ] >>= fun () -> install "result/bin/config"
  | NixDefault ->
    run [ "nix-build"; config_dir ] >>= fun () -> install "result/bin/config"
  | Dune ->
    run ~cwd:config [ "dune"; "build"; "bin/config.exe" ]
    >>= fun () -> install ~cwd:config "_build/default/bin/config.exe"
  | Ocamlopt ->
    let obj = Eio.Path.native_exn (cache / "config.cmx") in
    run
      [ "ocamlfind"
      ; "ocamlopt"
      ; "-package"
      ; "ghola"
      ; "-c"
      ; src_file_name dirs
      ; "-o"
      ; obj
      ]
    >>= fun () ->
    run
      [ "ocamlfind"
      ; "ocamlopt"
      ; "-package"
      ; "ghola"
      ; "-linkpkg"
      ; obj
      ; "-o"
      ; "config.exe"
      ]
    >>= fun () -> install "config.exe"
;;

let mtime path =
  match Eio.Path.stat ~follow:true path with
  | st -> Some st.Eio.File.Stat.mtime
  | exception _ -> None
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

let report_meth = function
  | Script -> print_endline "Ghola will use build script to recompile"
  | NixFlake -> print_endline "Ghola will use nix flake to recompile"
  | NixDefault -> print_endline "Ghola will use default.nix to recompile"
  | Dune -> print_endline "Ghola will use dune to recompile"
  | Ocamlopt -> print_endline "Ghola will use ocamlopt to recompile"
;;

let report_errors msg dirs =
  let errors = Directories.err_file_name dirs in
  Printf.eprintf
    "Errors detected while compiling Ghola config, check %s for full logs\n"
    errors;
  prerr_endline msg
;;

let recompile ?(force = false) mgr (dirs : _ Directories.t) =
  match detect_compile dirs.config with
  | None ->
    prerr_endline "Could not detect a compilation method, giving up";
    false
  | Some meth when (not force) && not (should_compile dirs meth) ->
    print_endline "Recompilation not necessary, moving on with existing config";
    true
  | Some meth ->
    report_meth meth;
    (match compile mgr dirs meth with
     | Ok () ->
       print_endline "Config compiled succesfully";
       true
     | Error msg ->
       report_errors msg dirs;
       false)
;;
