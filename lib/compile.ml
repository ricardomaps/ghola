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
    (fun (f, meth) -> Option.return_if (Eio.Path.is_file (dir / f)) meth)
    [ "build", Script
    ; "flake.nix", NixFlake
    ; "default.nix", NixDefault
    ; "dune-project", Dune
    ; "config.ml", Ocamlopt
    ]
;;

let compile mgr meth ~fs =
  let open Directories in
  let ( >>= ) = Result.( >>= ) in
  Eio.Path.with_open_out ~create:(`Or_truncate 0o644) (fs / err_file_name)
  @@ fun stderr ->
  let fail msg =
    Eio.Flow.copy_string (msg ^ "\n") stderr;
    Error msg
  in
  let install ?(cwd = fs / cache_dir) src =
    let tmp = cwd / "ghola-config.tmp" in
    try
      Eio.Path.unlink ~missing_ok:true tmp;
      Eio.Path.(load (cwd / src) |> save ~create:(`Exclusive 0o755) tmp);
      Eio.Path.rename tmp (fs / bin_file_name);
      Ok ()
    with
    | Eio.Io _ as e ->
      fail
      @@ Fmt.str
           "Failed to copy %a to %a: %a"
           Eio.Path.pp
           (cwd / src)
           Eio.Path.pp
           (fs / bin_file_name)
           Eio.Exn.pp
           e
  in
  let run ?(cwd = fs / cache_dir) cmd =
    try
      Eio.Process.run mgr ~cwd ~stderr ~stdout:stderr cmd;
      Ok ()
    with
    | Eio.Exn.Io (e, _) -> fail @@ Fmt.str "%a" Eio.Exn.pp_err e
  in
  match meth with
  | Script -> run [ config_dir ^ "/build"; bin_file_name ]
  | NixFlake ->
    run [ "nix"; "build"; config_dir ] >>= fun () -> install "result/bin/config"
  | NixDefault ->
    run [ "nix-build"; config_dir ] >>= fun () -> install "result/bin/config"
  | Dune ->
    let cwd = fs / config_dir in
    run ~cwd [ "dune"; "build"; "bin/config.exe" ]
    >>= fun () -> install ~cwd "_build/default/bin/config.exe"
  | Ocamlopt ->
    let obj = Eio.Path.native_exn (fs / cache_dir / "config.cmx") in
    run [ "ocamlfind"; "ocamlopt"; "-package"; "ghola"; "-c"; src_file_name; "-o"; obj ]
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

let should_compile ~fs = function
  | Ocamlopt ->
    let btime = fs / Directories.bin_file_name |> mtime in
    let stime = fs / Directories.src_file_name |> mtime in
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

let report_errors msg =
  let errors = Directories.err_file_name in
  Printf.eprintf
    "Errors detected while compiling Ghola config, check %s for full logs\n"
    errors;
  prerr_endline msg
;;

let recompile ?(force = false) mgr ~fs =
  match detect_compile (fs / Directories.config_dir) with
  | None ->
    prerr_endline "Could not detect a compilation method, giving up";
    false
  | Some meth when (not force) && not (should_compile ~fs meth) ->
    print_endline "Recompilation not necessary, moving on with existing config";
    true
  | Some meth ->
    report_meth meth;
    (match compile mgr meth ~fs with
     | Ok () ->
       print_endline "Config compiled succesfully";
       true
     | Error msg ->
       report_errors msg;
       false)
;;
