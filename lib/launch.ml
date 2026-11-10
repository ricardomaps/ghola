open Proto
open Handlers

let run env config = 
  let open Wayland in
  Eio.Switch.run
  @@ fun sw ->
  let transport = Unix_transport.connect ~sw ~net:env#net () in
  let display = Client.connect ~sw transport in
  let registry = Registry.of_display display in
  let rec wm : Wm.t Lazy.t =
    lazy
      { config
      ; env
      ; window_manager_proxy = Registry.bind registry (window_manager_handler wm)
      ; xkb_bindings_proxy = Registry.bind registry (new River_xkb_bindings.v2)
      ; layer_shell_proxy = Registry.bind registry (new River_layer_shell.v1)
      ; window_set = Window_set.create config.layout config.workspaces
      ; queue = Queue.create ()
      ; seats = []
      ; layer_focus = false
      ; hovered_window = None
      }
  in
  config.startup_hook (Lazy.force wm)
