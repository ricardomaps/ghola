open! Containers

let xdg = Xdg.create ~env:Sys.getenv_opt ()
let cache_dir = Xdg.cache_dir xdg ^ "/ghola"
let config_dir = Xdg.config_dir xdg ^ "/ghola"
let data_dir = Xdg.data_dir xdg ^ "/ghola"
let err_file_name = data_dir ^ "/ghola.errors"
let bin_file_name = cache_dir ^ "/ghola-bin"
let src_file_name = config_dir ^ "/config.ml"
let state_file_name = data_dir ^ "/ghola.state"

let ensure_exists ~fs =
  [ cache_dir; config_dir; data_dir ]
  |> List.map (fun dir -> Eio.Path.(fs / dir))
  |> List.iter (Eio.Path.mkdirs ~exists_ok:true ~perm:0o755)
;;
