open Common
open Handlers

let run env (config : _ Config.t) =
  let open Wayland in
  Eio.Switch.run
  @@ fun sw ->
  let transport = Unix_transport.connect ~sw ~net:env#net () in
  let display = Client.connect ~sw transport in
  let registry = Registry.of_display display in
  let window_set =
    try Wm.read_state config.layout with
    | _ -> Window_set.create config.layout config.workspaces
  in
  let rec wm : Wm.t Lazy.t =
    lazy
      { config
      ; env
      ; window_manager_proxy = Registry.bind registry (window_manager_handler wm)
      ; xkb_bindings_proxy = Registry.bind registry (new River_xkb_bindings.v2)
      ; layer_shell_proxy = Registry.bind registry (new River_layer_shell.v1)
      ; window_set
      ; restart = false
      ; windows = StringMap.empty
      ; outputs = Int32Map.empty
      ; queue = Queue.create ()
      ; seats = []
      ; layer_focus = false
      ; hovered_window = None
      }
  in
  let wm = Lazy.force wm in
  Wm.defer wm (fun () -> Wm.remove_dropped wm);
  config.startup_hook wm
;;

let start config = Eio_main.run @@ fun env -> run env config
