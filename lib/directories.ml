let ( / ) = Eio.Path.( / )

type 'a t =
  { config : 'a Eio.Path.t
  ; cache : 'a Eio.Path.t
  ; data : 'a Eio.Path.t
  }

let err_file { data; _ } = data / "ghola.errors"
let err_file_name dirs = Eio.Path.native_exn @@ err_file dirs
let bin_file { cache; _ } = cache / "ghola-bin"
let bin_file_name dirs = Eio.Path.native_exn @@ bin_file dirs
let lib_file { config; _ } = config / "lib"
let lib_file_name dirs = Eio.Path.native_exn @@ lib_file dirs
let src_file { config; _ } = config / "config.ml"
let src_file_name dirs = Eio.Path.native_exn @@ src_file dirs

let xdg_dirs ~fs =
  let xdg = Xdg.create ~env:Sys.getenv_opt () in
  { cache = fs / Xdg.cache_dir xdg / "ghola"
  ; config = fs / Xdg.config_dir xdg / "ghola"
  ; data = fs / Xdg.data_dir xdg / "ghola"
  }
;;

let get ~fs =
  let ({ cache; config; data } as dirs) = xdg_dirs ~fs in
  List.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755) [ cache; config; data ];
  dirs
;;
