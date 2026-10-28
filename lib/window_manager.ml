open Wm
open Containers

module River_window_manager =
  River.River_window_management_v1_client.River_window_manager_v1

module River_window = River.River_window_management_v1_client.River_window_v1
module River_xkb_bindings = River.River_xkb_bindings_v1_client.River_xkb_bindings_v1
module River_xkb_binding = River.River_xkb_bindings_v1_client.River_xkb_binding_v1
module River_seat = River.River_window_management_v1_client.River_seat_v1
module River_output = River.River_window_management_v1_client.River_output_v1
module River_node = River.River_window_management_v1_client.River_node_v1
module Edges = River.River_window_management_v1_client.River_window_v1.Edges

module River_pointer_binding =
  River.River_window_management_v1_client.River_pointer_binding_v1

type 'a user_data =
  | Window : window -> [ `River_window_v1 ] user_data
  | Output : output -> [ `River_output_v1 ] user_data

type ('a, 'role) Wayland.S.user_data +=
  | User_data : 'a user_data -> ('a, [ `Client ]) Wayland.S.user_data

let user_data (proxy : ('a, _, 'role) Wayland.Proxy.t) : 'a user_data =
  match Wayland.Proxy.user_data proxy with
  | User_data x -> x
  | Wayland.S.No_data -> Fmt.failwith "No data attached to %a!" Wayland.Proxy.pp proxy
  | _ -> Fmt.failwith "Unexpected data attached to %a!" Wayland.Proxy.pp proxy
;;

let all_edges =
  let open Int32 in
  Edges.(bottom lor top lor left lor right)
;;

(* this is a very weird function and river's color representation is also weird to me *)
let to_river_color rgba =
  let r = (rgba lsr 24) land 0xff in
  let g = (rgba lsr 16) land 0xff in
  let b = (rgba lsr 8) land 0xff in
  let a = rgba land 0xff in
  let premul c = ((c * a) + 127) / 255 in
  let widen v = Int32.mul (Int32.of_int v) 0x01010101l in
  widen (premul r), widen (premul g), widen (premul b), widen a
;;

let spawn cmd wm =
  Eio.Process.run
    wm.env#process_mgr
    [ "sh"; "-c"; {|setsid -f sh -c "$1" </dev/null >/dev/null 2>&1|}; "spawn"; cmd ]
;;

let windows f wm = wm.window_set <- f wm.window_set

let kill_focused_window wm =
  match Window_set.focused_window wm.window_set with
  | None -> ()
  | Some window -> Queue.add (fun () -> River_window.close window.proxy) wm.queue
;;

let send_message message wm =
  wm.window_set
  <- Window_set.map_current_workspace
       (fun current ->
          let (Layout ((module L), layout)) = current.layout in
          match L.handle_message wm layout message with
          | None -> current
          | Some layout -> { current with layout = Layout ((module L), layout) })
       wm.window_set
;;

let exit_wm wm = River_window_manager.stop wm.window_manager_proxy

let keys =
  let open River_seat.Modifiers in
  let open Xkbcommon.Keysym in
  let open Int32.Infix in
  let open Layouts in
  Hashtbl.of_list
    [ (mod4, K_Down), windows Window_set.focus_down
    ; (mod4, K_Up), windows Window_set.focus_up
    ; (mod4 lor shift, K_Down), windows Window_set.swap_down
    ; (mod4 lor shift, K_Up), windows Window_set.swap_up
    ; (mod4 lor shift, K_c), kill_focused_window
    ; (mod4, K_m), windows Window_set.focus_master
    ; (mod4 lor shift, K_m), windows Window_set.swap_master
    ; (mod4, K_Left), send_message Shrink
    ; (mod4, K_Right), send_message Expand
    ; (mod4, K_comma), send_message (IncMasterN 1)
    ; (mod4, K_period), send_message (IncMasterN (-1))
    ; (mod4, K_space), send_message NextLayout
    ; (mod4, K_t), spawn "alacritty"
    ; (mod4 lor shift, K_q), exit_wm
    ]
;;

let layout =
  let open Layouts in
  let tiled_layout = tall 1 0.03 0.5 in
  tiled_layout ||| mirror tiled_layout ||| full
;;

let config =
  { keys
  ; workspaces = List.range 0 9 |> List.map Int.to_string
  ; layout
  ; focus_follows_mouse = false
  ; border_width = 2
  ; normal_border_color = 0
  ; focused_border_color = 0xFF0000FF
  }
;;

let xkb_binding_listener wm action =
  object
    inherit [_] River_xkb_binding.v2
    method bind_version = `V2
    method on_pressed _ = action wm
    method on_stop_repeat id = ()
    method on_released id = ()
  end
;;

let output_listener wm output =
  object
    inherit [_] River_output.v4
    method bind_version = `V4
    method user_data = User_data (Output output)

    method on_removed _ =
      River_output.destroy output.proxy;
      Wayland.Proxy.delete output.proxy;
      wm.window_set <- Window_set.remove_screen output wm.window_set

    method on_wl_output _ ~name:_ = ()

    method on_position data ~x ~y =
      let x, y = Int32.(to_int x, to_int y) in
      output.geometry <- { output.geometry with x; y }

    method on_dimensions data ~width ~height =
      let w, h = Int32.(to_int width, to_int height) in
      output.geometry <- { output.geometry with w; h }
  end
;;

let seat_listener wm seat =
  object
    inherit [_] River_seat.v4
    method bind_version = `V4

    method on_window_interaction proxy ~window =
      let (Window window) = user_data window in
      match Window_set.focus_window window wm.window_set with
      | Some window_set -> wm.window_set <- window_set
      | None -> ()

    method on_removed _ =
      List.iter
        (fun binding ->
           River_xkb_binding.destroy binding;
           Wayland.Proxy.delete binding)
        seat.bindings;
      River_seat.destroy seat.proxy;
      Wayland.Proxy.delete seat.proxy;
      wm.seats <- List.remove_one ~eq:Stdlib.( == ) seat wm.seats

    method on_pointer_leave proxy = ()

    method on_pointer_enter proxy ~window =
      let (Window window) = user_data window in
      if wm.config.focus_follows_mouse
      then (
        match Window_set.focus_window window wm.window_set with
        | Some window_set -> wm.window_set <- window_set
        | None -> ())

    method on_op_release proxy = ()
    method on_op_delta proxy ~dx ~dy = ()
    method on_wl_seat _ ~name:_ = ()
    method on_pointer_position _ ~x:_ ~y:_ = ()
    method on_shell_surface_interaction _ ~shell_surface:_ = ()
  end
;;

let window_listener wm window =
  object
    inherit [_] River_window.v4
    method bind_version = `V4
    method user_data = User_data (Window window)
    method on_pointer_resize_requested data ~seat ~edges = ()
    method on_pointer_move_requested data ~seat = ()

    method on_dimensions data ~width ~height =
      let w, h = Int32.(to_int width, to_int height) in
      window.dimensions <- { window.dimensions with w; h }

    method on_closed _ =
      River_window.destroy window.proxy;
      Wayland.Proxy.delete window.proxy;
      wm.window_set <- Window_set.remove_window window wm.window_set

    method on_unreliable_pid _ ~unreliable_pid:_ = ()
    method on_unmaximize_requested _ = ()
    method on_title data ~title = window.title <- title
    method on_show_window_menu_requested _ ~x ~y = ()
    method on_presentation_hint _ ~hint = ()
    method on_decoration_hint _ ~hint = ()
    method on_app_id data ~app_id = window.app_id <- app_id
    method on_parent data ~parent = ()
    method on_minimize_requested _ = ()
    method on_maximize_requested _ = ()
    method on_identifier data ~identifier = window.identifier <- Some identifier
    method on_fullscreen_requested _ ~output:_ = ()
    method on_exit_fullscreen_requested _ = ()
    method on_dimensions_hint _ ~min_width:_ ~min_height:_ ~max_width:_ ~max_height:_ = ()
  end
;;

let window_manager_listener wm =
  object
    inherit [_] River_window_manager.v4

    method on_unavailable _ =
      Stdlib.print_endline "Error: Another WM is already running";
      Stdlib.exit 1

    method on_window data proxy =
      let (lazy wm) = wm in
      let window =
        { proxy
        ; node = River_window.get_node proxy (new River_node.v4)
        ; dimensions = { x = 0; y = 0; w = 0; h = 0 }
        ; title = None
        ; app_id = None
        ; identifier = None
        ; parent = None
        }
      in
      wm.window_set <- Window_set.insert_up window wm.window_set;
      Queue.add (fun () -> River_window.use_ssd window.proxy) wm.queue;
      Wayland.Proxy.Handler.attach proxy (window_listener wm window)

    method on_seat _ proxy =
      let (lazy wm) = wm in
      let bindings =
        Hashtbl.map_list
          (fun (modifiers, keysym) action ->
             let keysym = Int32.of_int @@ Xkbcommon.Keysym.to_int keysym in
             River_xkb_bindings.get_xkb_binding
               wm.xkb_bindings_proxy
               (xkb_binding_listener wm action)
               ~seat:proxy
               ~modifiers
               ~keysym)
          wm.config.keys
      in
      let seat = { proxy; bindings } in
      wm.seats <- wm.seats @ [ seat ];
      Wayland.Proxy.Handler.attach proxy (seat_listener wm seat);
      Queue.add (fun () -> List.iter River_xkb_binding.enable seat.bindings) wm.queue

    method on_output _ proxy =
      let (lazy wm) = wm in
      let empty : Rect.t = { x = 0; y = 0; w = 0; h = 0 } in
      let output = { proxy; geometry = empty } in
      wm.window_set <- Window_set.add_screen output wm.window_set;
      Wayland.Proxy.Handler.attach proxy (output_listener wm output)

    method on_render_start proxy = River_window_manager.render_finish proxy

    (* TODO: this method is getting very bulky, prob needs breaking up *)
    method on_manage_start _ =
      let (lazy wm) = wm in
      (* run the queued actions *)
      Queue.iter (fun action -> action ()) wm.queue;
      Queue.clear wm.queue;
      (* layout and update the workspaces *)
      let rects, visible =
        List.fold_map
          (fun acc (screen : (_, _, output) Window_set.screen) ->
             let (Layout ((module L), l)) = screen.workspace.layout in
             let workspace = { screen.workspace with layout = l } in
             let rects, l = L.run_layout wm workspace screen.output.geometry in
             let screen =
               Option.map_or
                 ~default:screen
                 (fun l ->
                    { screen with
                      workspace = { workspace with layout = Layout ((module L), l) }
                    })
                 l
             in
             rects @ acc, screen)
          []
          wm.window_set.visible
      in
      wm.window_set <- { wm.window_set with visible };
      (* set the geometry of the windows according to the layout *)
      let rects, visible_windows = List.split rects in
      List.iter2
        (fun (rect : Rect.t) window ->
           let open Int32 in
           let bw = Int32.of_int wm.config.border_width in
           let x, y, width, height =
             Int32.(
               ( of_int rect.x + bw
               , of_int rect.y + bw
               , of_int rect.w - (2l * bw)
               , of_int rect.h - (2l * bw) ))
           in
           River_window.propose_dimensions window.proxy ~width ~height;
           River_node.set_position window.node ~x ~y;
           River_window.show window.proxy;
           River_window.set_tiled window.proxy ~edges:all_edges;
           window.dimensions <- rect)
        rects
        visible_windows;
      (* set the border for the windows *)
      let r, g, b, a = to_river_color wm.config.normal_border_color in
      List.iter
        (fun window ->
           River_window.set_borders
             window.proxy
             ~edges:all_edges
             ~width:(Int32.of_int wm.config.border_width)
             ~r
             ~g
             ~b
             ~a)
        visible_windows;
      (* hide all non-visible windows *)
      Window_set.all_windows wm.window_set
      |> List.filter (fun window -> not @@ List.memq window visible_windows)
      |> List.iter (fun window -> River_window.hide window.proxy);
      (* manage focused windowed *)
      (match Window_set.focused_window wm.window_set with
       | None -> ()
       | Some window ->
         let r, g, b, a = to_river_color wm.config.focused_border_color in
         River_window.set_borders
           window.proxy
           ~edges:all_edges
           ~width:(Int32.of_int wm.config.border_width)
           ~r
           ~g
           ~b
           ~a;
         List.iter
           (fun (seat : seat) -> River_seat.focus_window seat.proxy ~window:window.proxy)
           wm.seats);
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
  let rec wm =
    lazy
      { config
      ; env
      ; window_manager_proxy = Registry.bind registry (window_manager_listener wm)
      ; xkb_bindings_proxy = Registry.bind registry (new River_xkb_bindings.v2)
      ; window_set = Window_set.create config.layout config.workspaces []
      ; queue = Queue.create ()
      ; seats = []
      }
  in
  ignore @@ Lazy.force wm
;;
