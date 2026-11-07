open Proto
open Containers

type 'a user_data =
  | Window : Window.t -> [ `River_window_v1 ] user_data
  | Output : Output.t -> [ `River_output_v1 ] user_data

type ('a, 'role) Wayland.S.user_data +=
  | User_data : 'a user_data -> ('a, [ `Client ]) Wayland.S.user_data

let user_data (proxy : ('a, _, 'role) Wayland.Proxy.t) : 'a user_data =
  match Wayland.Proxy.user_data proxy with
  | User_data x -> x
  | Wayland.S.No_data -> Fmt.failwith "No data attached to %a!" Wayland.Proxy.pp proxy
  | _ -> Fmt.failwith "Unexpected data attached to %a!" Wayland.Proxy.pp proxy
;;

let xkb_binding_handler wm action =
  object
    inherit [_] River_xkb_binding.v2
    method bind_version = `V2
    method on_pressed _ = action wm
    method on_stop_repeat id = ()
    method on_released id = ()
  end
;;

let output_handler (wm : Wm.t) output =
  object
    inherit [_] River_output.v4
    method bind_version = `V4
    method user_data = User_data (Output output)

    method on_removed _ =
      Output.destroy output;
      wm.window_set <- Window_set.remove_screen output wm.window_set

    method on_wl_output _ ~name:_ = ()
    method on_position data ~x ~y = output.geometry <- { output.geometry with x; y }

    method on_dimensions data ~width ~height =
      output.geometry <- { output.geometry with w = width; h = height }
  end
;;

let shell_output_handler wm output_proxy =
  object
    inherit [_] River_layer_shell_output.v1

    method on_non_exclusive_area _ ~x ~y ~width ~height =
      let (Output output) = user_data output_proxy in
      output.working_geometry <- { x; y; w = width; h = height }
  end
;;

let seat_handler (wm : Wm.t) seat =
  object
    inherit [_] River_seat.v4
    method bind_version = `V4

    method on_window_interaction proxy ~window =
      let (Window window) = user_data window in
      Wm.windows (Window_set.focus_window window) wm

    method on_removed _ =
      Seat.destroy seat;
      wm.seats <- List.remove_one ~eq:Seat.equal seat wm.seats

    method on_pointer_leave proxy = wm.hovered_window <- None

    method on_pointer_enter proxy ~window =
      let (Window window) = user_data window in
      wm.hovered_window <- Some window;
      if wm.config.focus_follows_mouse then Wm.windows (Window_set.focus_window window) wm

    method on_op_release _ = Wm.cancel_operation seat wm

    method on_op_delta proxy ~dx ~dy =
      Option.iter (Operation.set_delta ~dx ~dy) seat.operation

    method on_wl_seat _ ~name:_ = ()
    method on_pointer_position _ ~x:_ ~y:_ = ()
    method on_shell_surface_interaction _ ~shell_surface:_ = ()
  end
;;

let shell_seat_handler (wm : Wm.t) =
  object
    inherit [_] River_layer_shell_seat.v1
    method on_focus_none _ = wm.layer_focus <- false
    method on_focus_exclusive _ = wm.layer_focus <- true
    method on_focus_non_exclusive _ = wm.layer_focus <- true
  end
;;

let window_handler wm window =
  object
    inherit [_] River_window.v4
    method bind_version = `V4
    method user_data = User_data (Window window)
    method on_pointer_resize_requested _ ~seat ~edges = ()
    method on_pointer_move_requested _ ~seat:_ = Wm.float window wm

    method on_dimensions data ~width ~height =
      window.dimensions <- { window.dimensions with w = width; h = height }

    method on_closed _ =
      Window.destroy window;
      Wm.windows (Window_set.remove_window window) wm;
      List.iter
        (fun (seat : Seat.t) ->
           let w_opt = Option.map Operation.window seat.operation in
           if Option.equal Window.equal w_opt (Some window)
           then Wm.cancel_operation seat wm)
        wm.seats;
      if Option.equal Window.equal wm.hovered_window (Some window)
      then wm.hovered_window <- None

    method on_unreliable_pid _ ~unreliable_pid:_ = ()
    method on_unmaximize_requested _ = ()
    method on_title data ~title = window.title <- title
    method on_show_window_menu_requested _ ~x ~y = ()
    method on_presentation_hint _ ~hint = ()
    method on_decoration_hint _ ~hint = ()
    method on_app_id data ~app_id = window.app_id <- app_id

    method on_parent _ ~parent =
      match parent with
      | None -> ()
      | Some parent_proxy ->
        let (Window parent) = user_data parent_proxy in
        window.parent <- Some parent

    method on_minimize_requested _ = ()
    method on_maximize_requested _ = ()
    method on_identifier data ~identifier = window.identifier <- Some identifier
    method on_fullscreen_requested _ ~output:_ = ()
    method on_exit_fullscreen_requested _ = ()

    method on_dimensions_hint _ ~min_width ~min_height ~max_width ~max_height =
      window.min_width <- min_width;
      window.max_width <- max_width;
      window.min_height <- min_height;
      window.max_height <- max_height
  end
;;

let pointer_binding_handler (wm : Wm.t) action =
  object
    inherit [_] River_pointer_binding.v4
    method on_released data = ()

    method on_pressed data =
      Option.iter (fun window -> action window wm) wm.hovered_window
  end
;;

let window_manager_handler (wm : Types.Wm.t Lazy.t) =
  object
    inherit [_] River_window_manager.v4

    method on_unavailable _ =
      Stdlib.print_endline "Error: Another WM is already running";
      Stdlib.exit 1

    method on_window data proxy =
      let (lazy wm) = wm in
      let window = Window.create proxy in
      Wm.windows (Window_set.insert_up window) wm;
      Wm.defer wm (fun () -> Wm.init_window window wm);
      Wayland.Proxy.Handler.attach proxy (window_handler wm window)

    method on_seat _ proxy =
      let (lazy wm) = wm in
      let seat =
        Seat.create
          proxy
          ~keys:wm.config.keys
          ~pointer_bindings:wm.config.pointer_bindings
          ~seat_handler:(seat_handler wm)
          ~xkb_handler:(xkb_binding_handler wm)
          ~pointer_handler:(pointer_binding_handler wm)
          ~shell_handler:(shell_seat_handler wm)
          ~bindings_proxy:wm.xkb_bindings_proxy
          ~shell_proxy:wm.layer_shell_proxy
      in
      Wm.add_seat seat wm;
      Wm.defer wm (fun () ->
        List.iter River_xkb_binding.enable seat.bindings;
        List.iter River_pointer_binding.enable seat.pointer_bindings)

    method on_output _ proxy =
      let (lazy wm) = wm in
      let output =
        Output.create
          proxy
          ~output_handler:(output_handler wm)
          ~shell_proxy:wm.layer_shell_proxy
          ~shell_handler:(shell_output_handler wm)
      in
      wm.window_set <- Window_set.add_screen output wm.window_set

    method on_render_start proxy =
      let (lazy wm) = wm in
      Wm.render_start wm;
      River_window_manager.render_finish proxy

    method on_manage_start _ =
      let (lazy wm) = wm in
      Wm.manage_start wm;
      River_window_manager.manage_finish wm.window_manager_proxy

    method on_finished _ = Stdlib.exit 0
    method on_session_unlocked _ = ()
    method on_session_locked _ = ()
  end
;;

let run env =
  let open Wayland in
  Eio.Switch.run
  @@ fun sw ->
  let transport = Unix_transport.connect ~sw ~net:env#net () in
  let display = Client.connect ~sw transport in
  let registry = Registry.of_display display in
  let config = Config.default in
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
;;
