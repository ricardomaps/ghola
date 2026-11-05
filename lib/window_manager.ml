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
module River_layer_shell = River.River_layer_shell_v1_client.River_layer_shell_v1
module Seat_layer_shell = River.River_layer_shell_v1_client.River_layer_shell_seat_v1
module Output_layer_shell = River.River_layer_shell_v1_client.River_layer_shell_output_v1

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

let has_edge edge edges =
  let open Int32 in
  let open Edges in
  to_int32 edges land to_int32 edge > 0l
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

let operation_window = function
  | Move { window; _ } | Resize { window; _ } -> window
;;

let cancel_operation seat wm =
  (match seat.operation with
   | None -> ()
   | Some (Resize { window; _ }) ->
     Queue.add
       (fun () ->
          River_seat.op_end seat.proxy;
          River_window.inform_resize_end window.proxy)
       wm.queue
   | Some (Move _) -> Queue.add (fun () -> River_seat.op_end seat.proxy) wm.queue);
  seat.operation <- None
;;

let screen_at x y (window_set : _ Window_set.t) =
  window_set.visible
  |> List.find_opt (fun (screen : _ Window_set.screen) ->
    Rect.is_point_within x y screen.output.geometry)
;;

let clamp_dim ~lo ~hi v =
  let v = if lo > 0 then max v lo else v in
  if hi > 0 then min v hi else v
;;

let clamp_size window ~w ~h =
  ( clamp_dim ~lo:(Int32.to_int window.min_width) ~hi:(Int32.to_int window.max_width) w
  , clamp_dim ~lo:(Int32.to_int window.min_height) ~hi:(Int32.to_int window.max_height) h
  )
;;

let relative_of (d : Rect.t) (screen : Rect.t) =
  let f = Float.of_int in
  { cx = (f d.x +. (f d.w /. 2.) -. f screen.x) /. f screen.w
  ; cy = (f d.y +. (f d.h /. 2.) -. f screen.y) /. f screen.h
  ; rw = f d.w /. f screen.w
  ; rh = f d.h /. f screen.h
  }
;;

let place_floating window rel (screen : Rect.t) bw : Rect.t =
  let f = Float.of_int in
  let px v = Int.of_float (Float.round v) in
  let iw, ih =
    clamp_size window ~w:(px (rel.rw *. f screen.w)) ~h:(px (rel.rh *. f screen.h))
  in
  let w = iw + (2 * bw)
  and h = ih + (2 * bw) in
  let cx = screen.x + px (rel.cx *. f screen.w) in
  let cy = screen.y + px (rel.cy *. f screen.h) in
  { x = cx - (w / 2); y = cy - (h / 2); w; h }
;;

let float window wm =
  match wm.window_set.visible with
  | [] -> ()
  | current :: _ ->
    let d = window.dimensions in
    let screen =
      Option.value
        ~default:current
        (screen_at (d.x + (d.w / 2)) (d.y + (d.h / 2)) wm.window_set)
    in
    wm.window_set <- Window_set.shift_window screen.workspace.tag window wm.window_set;
    window.presentation <- Floating (relative_of d screen.output.geometry)
;;

let initial_floating = { cx = 0.5; cy = 0.5; rw = 0.5; rh = 0.5 }

let tile_window wm =
  Window_set.focused_window wm.window_set
  |> Option.iter (fun w -> w.presentation <- Tiled)
;;

let float_window wm =
  Window_set.focused_window wm.window_set |> Option.iter (fun w -> float w wm)
;;

let is_fixed_sized window =
  let open Int32 in
  window.max_height <> 0l
  && window.max_height = window.min_height
  && window.max_width <> 0l
  && window.max_width = window.min_width
;;

let is_tiled window =
  match window.presentation with
  | Tiled -> true
  | _ -> false
;;

let is_floating window =
  match window.presentation with
  | Floating _ -> true
  | _ -> false
;;

let output_geometry wm =
  match wm.window_set.visible with
  | { output; _ } :: _ -> Some output.geometry
  | _ -> None
;;

let center_rect ~min_w ~max_w ~min_h ~max_h (r : Rect.t) : Rect.t =
  let dim ~lo ~hi ~limit =
    let open Int32 in
    let v = if lo > 0l then lo else limit / 2l in
    let v = if hi > 0l then min v hi else v in
    min v limit
  in
  let w = Int32.to_int @@ dim ~lo:min_w ~hi:max_w ~limit:(Int32.of_int r.w) in
  let h = Int32.to_int @@ dim ~lo:min_h ~hi:max_h ~limit:(Int32.of_int r.h) in
  let x = r.x + ((r.w - w) / 2) in
  let y = r.y + ((r.h - h) / 2) in
  { w; h; x; y }
;;

let parent_workspace window wm =
  Option.bind window.parent (fun parent -> Window_set.find_tag parent wm.window_set)
;;

let should_float (window : window) = is_fixed_sized window || Option.is_some window.parent

let init_window (window : window) wm =
  River_window.use_ssd window.proxy;
  (* send a transient window to its parent's workspace *)
  parent_workspace window wm
  |> Option.iter (fun tag -> windows (Window_set.shift_window tag window) wm);
  if should_float window then window.presentation <- Floating initial_floating
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

let exit_wm wm = River_window_manager.exit_session wm.window_manager_proxy
let workspaces = List.range 0 9 |> List.map Int.to_string

let keys =
  let open River_seat.Modifiers in
  let open Xkbcommon.Keysym in
  let open Int32.Infix in
  let open Layouts in
  let digit_keys = [ K_0; K_1; K_2; K_3; K_4; K_5; K_6; K_7; K_8; K_9 ] in
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
  ; (mod4, K_n), send_message NextLayout
  ; (mod4, K_t), tile_window
  ; (mod4, K_f), float_window
  ; (mod4, K_Return), spawn "alacritty"
  ; (mod4, K_space), spawn "noctalia msg panel-toggle launcher"
  ; (mod4, K_XF86AudioRaiseVolume), spawn "noctalia msg volume-up"
  ; (mod4, K_XF86AudioLowerVolume), spawn "noctalia msg volume-down"
  ; (mod4, K_XF86AudioMute), spawn "noctalia msg volume-mute"
  ; (mod4, K_XF86MonBrightnessUp), spawn "noctalia msg brightness-up"
  ; (mod4, K_XF86MonBrightnessDown), spawn "noctalia msg brightness-down"
  ; (mod4 lor shift, K_q), exit_wm
  ]
  @ List.map2
      (fun workspace key -> (mod4, key), windows (Window_set.view workspace))
      workspaces
      digit_keys
  @ List.map2
      (fun workspace key -> (mod4 lor shift, key), windows (Window_set.shift workspace))
      workspaces
      digit_keys
;;

let seat wm = List.hd wm.seats

let resize_window window wm =
  let seat = seat wm in
  float window wm;
  let operation =
    Resize
      { window
      ; edges = Int32.( lor ) Edges.bottom Edges.top
      ; dx = 0l
      ; dy = 0l
      ; start_rect = window.dimensions
      }
  in
  seat.operation <- Some operation;
  Queue.add
    (fun () ->
       River_seat.op_start_pointer seat.proxy;
       River_window.inform_resize_start window.proxy)
    wm.queue
;;

let move_window window wm =
  let seat = seat wm in
  float window wm;
  let operation =
    Move
      { window
      ; start_x = Int32.of_int window.dimensions.x
      ; start_y = Int32.of_int window.dimensions.y
      ; dx = 0l
      ; dy = 0l
      }
  in
  seat.operation <- Some operation;
  Queue.add (fun () -> River_seat.op_start_pointer seat.proxy) wm.queue
;;

let pointer_bindings =
  let open River_seat.Modifiers in
  let open Window_set in
  let button1, button2 = 0x110l, 0x111l in
  [ ( (mod4, button1)
    , fun window wm ->
        windows (focus_window window) wm;
        move_window window wm;
        windows shift_master wm )
  ; ( (mod4, button2)
    , fun window wm ->
        windows (focus_window window) wm;
        resize_window window wm;
        windows shift_master wm )
  ]
;;

let layout =
  let open Layouts in
  let tiled_layout = tall 1 0.03 0.5 in
  tiled_layout ||| mirror tiled_layout ||| full
;;

let config =
  { keys
  ; pointer_bindings
  ; layout
  ; workspaces
  ; focus_follows_mouse = false
  ; border_width = 2
  ; normal_border_color = 0x000000FF
  ; focused_border_color = 0xFF0000FF
  ; startup_hook = (fun _ -> ())
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
      Output_layer_shell.destroy output.layer_shell_proxy;
      Wayland.Proxy.delete output.layer_shell_proxy;
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

let layer_shell_output_listener wm output_proxy =
  object
    inherit [_] Output_layer_shell.v1

    method on_non_exclusive_area _ ~x ~y ~width ~height =
      let (Output output) = user_data output_proxy in
      let x, y, w, h = Int32.(to_int x, to_int y, to_int width, to_int height) in
      output.working_geometry <- { x; y; w; h }
  end
;;

let seat_listener wm seat =
  object
    inherit [_] River_seat.v4
    method bind_version = `V4

    method on_window_interaction proxy ~window =
      let (Window window) = user_data window in
      windows (Window_set.focus_window window) wm

    method on_removed _ =
      List.iter
        (fun binding ->
           River_xkb_binding.destroy binding;
           Wayland.Proxy.delete binding)
        seat.bindings;
      List.iter
        (fun pointer_binding ->
           River_pointer_binding.destroy pointer_binding;
           Wayland.Proxy.delete pointer_binding)
        seat.pointer_bindings;
      River_seat.destroy seat.proxy;
      Wayland.Proxy.delete seat.proxy;
      wm.seats <- List.remove_one ~eq:Stdlib.( == ) seat wm.seats

    method on_pointer_leave proxy = wm.hovered_window <- None

    method on_pointer_enter proxy ~window =
      let (Window window) = user_data window in
      wm.hovered_window <- Some window;
      if wm.config.focus_follows_mouse then windows (Window_set.focus_window window) wm

    method on_op_release _ = cancel_operation seat wm

    method on_op_delta proxy ~dx ~dy =
      match seat.operation with
      | None -> ()
      | Some (Resize data) ->
        data.dx <- dx;
        data.dy <- dy
      | Some (Move data) ->
        data.dx <- dx;
        data.dy <- dy

    method on_wl_seat _ ~name:_ = ()
    method on_pointer_position _ ~x:_ ~y:_ = ()
    method on_shell_surface_interaction _ ~shell_surface:_ = ()
  end
;;

let layer_shell_seat_listener wm =
  object
    inherit [_] Seat_layer_shell.v1
    method on_focus_none _ = wm.layer_focus <- false
    method on_focus_exclusive _ = wm.layer_focus <- true
    method on_focus_non_exclusive _ = wm.layer_focus <- true
  end
;;

let window_listener wm window =
  object
    inherit [_] River_window.v4
    method bind_version = `V4
    method user_data = User_data (Window window)
    method on_pointer_resize_requested _ ~seat ~edges = ()
    method on_pointer_move_requested _ ~seat:_ = float window wm

    method on_dimensions data ~width ~height =
      let w, h = Int32.(to_int width, to_int height) in
      window.dimensions <- { window.dimensions with w; h }

    method on_closed _ =
      River_window.destroy window.proxy;
      Wayland.Proxy.delete window.proxy;
      wm.window_set <- Window_set.remove_window window wm.window_set;
      List.iter
        (fun seat ->
           let w_opt = Option.map operation_window seat.operation in
           if Option.equal Equal.physical w_opt (Some window)
           then cancel_operation seat wm)
        wm.seats;
      if Option.equal Equal.physical wm.hovered_window (Some window)
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

let pointer_binding_listener action wm =
  object
    inherit [_] River_pointer_binding.v4
    method on_released data = ()

    method on_pressed data =
      Option.iter (fun window -> action window wm) wm.hovered_window
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
        ; presentation = Tiled
        ; node = River_window.get_node proxy (new River_node.v4)
        ; dimensions = { x = 0; y = 0; w = 0; h = 0 }
        ; title = None
        ; app_id = None
        ; identifier = None
        ; parent = None
        ; min_width = 0l
        ; max_width = 0l
        ; min_height = 0l
        ; max_height = 0l
        }
      in
      wm.window_set <- Window_set.insert_up window wm.window_set;
      Queue.add (fun () -> init_window window wm) wm.queue;
      Wayland.Proxy.Handler.attach proxy (window_listener wm window)

    method on_seat _ proxy =
      let (lazy wm) = wm in
      let bindings =
        List.map
          (fun ((modifiers, keysym), action) ->
             let keysym = Int32.of_int @@ Xkbcommon.Keysym.to_int keysym in
             River_xkb_bindings.get_xkb_binding
               wm.xkb_bindings_proxy
               (xkb_binding_listener wm action)
               ~seat:proxy
               ~modifiers
               ~keysym)
          wm.config.keys
      in
      let pointer_bindings =
        List.map
          (fun ((modifiers, button), action) ->
             River_seat.get_pointer_binding
               proxy
               (pointer_binding_listener action wm)
               ~modifiers
               ~button)
          wm.config.pointer_bindings
      in
      let layer_shell_proxy =
        River_layer_shell.get_seat
          wm.layer_shell_proxy
          ~seat:proxy
          (layer_shell_seat_listener wm)
      in
      let seat =
        { proxy; bindings; layer_shell_proxy; operation = None; pointer_bindings }
      in
      wm.seats <- wm.seats @ [ seat ];
      Wayland.Proxy.Handler.attach proxy (seat_listener wm seat);
      Queue.add
        (fun () ->
           List.iter River_xkb_binding.enable seat.bindings;
           List.iter River_pointer_binding.enable seat.pointer_bindings)
        wm.queue

    method on_output _ proxy =
      let (lazy wm) = wm in
      let empty : Rect.t = { x = 0; y = 0; w = 0; h = 0 } in
      let layer_shell_proxy =
        River_layer_shell.get_output
          wm.layer_shell_proxy
          ~output:proxy
          (layer_shell_output_listener wm proxy)
      in
      let output =
        { proxy; geometry = empty; layer_shell_proxy; working_geometry = empty }
      in
      wm.window_set <- Window_set.add_screen output wm.window_set;
      Wayland.Proxy.Handler.attach proxy (output_listener wm output)

    method on_render_start proxy =
      let (lazy wm) = wm in
      List.filter_map (fun s -> s.operation) wm.seats
      |> List.iter (fun operation ->
        match operation with
        | Move { window; start_x; start_y; dx; dy } ->
          let open Int32 in
          window.dimensions
          <- { window.dimensions with
               x = to_int (dx + start_x)
             ; y = to_int (dy + start_y)
             };
          float window wm;
          River_node.set_position window.node ~x:(start_x + dx) ~y:(start_y + dy)
        | Resize { window; start_rect = { x; y; w; h }; edges; _ } ->
          let x = if has_edge Edges.left edges then x + w - window.dimensions.w else x in
          let y = if has_edge Edges.top edges then y + h - window.dimensions.h else y in
          window.dimensions <- { window.dimensions with x; y };
          float window wm;
          River_node.set_position window.node ~x:(Int32.of_int x) ~y:(Int32.of_int y));
      River_window_manager.render_finish proxy

    (* TODO: this method is getting very bulky, prob needs breaking up *)
    method on_manage_start _ =
      let (lazy wm) = wm in
      (* run the queued actions *)
      Queue.iter (fun action -> action ()) wm.queue;
      Queue.clear wm.queue;
      List.iter
        (fun (seat : seat) ->
           match seat.operation with
           | None -> ()
           | Some (Move data) -> ()
           | Some (Resize { window; dx; dy; start_rect = { w; h; _ }; edges }) ->
             let open Int32 in
             let open Edges in
             let w =
               of_int w
               +
               if has_edge left edges
               then -dx
               else if has_edge right edges
               then dx
               else 0l
             in
             let h =
               of_int h
               +
               if has_edge top edges
               then -dy
               else if has_edge bottom edges
               then dy
               else 0l
             in
             window.dimensions <- { window.dimensions with h = to_int h; w = to_int w };
             float window wm)
        wm.seats;
      (* layout and update the workspaces *)
      let tiles, visible =
        List.fold_map
          (fun acc (screen : (_, _, output) Window_set.screen) ->
             let (Layout ((module L), l)) = screen.workspace.layout in
             let tiled =
               Option.bind screen.workspace.stack (fun stack ->
                 Zipper.filter is_tiled stack)
             in
             let workspace = { screen.workspace with layout = l; stack = tiled } in
             let tiles, l = L.run_layout wm workspace screen.output.working_geometry in
             let floats =
               Option.map_or ~default:[] Zipper.to_list screen.workspace.stack
               |> List.filter_map (fun window ->
                 match window.presentation with
                 | Tiled -> None
                 | Floating rr ->
                   let rect =
                     place_floating
                       window
                       rr
                       screen.output.geometry
                       wm.config.border_width
                   in
                   Some (rect, window))
             in
             let screen =
               Option.map_or
                 ~default:screen
                 (fun l ->
                    { screen with
                      workspace = { workspace with layout = Layout ((module L), l) }
                    })
                 l
             in
             floats @ tiles @ acc, screen)
          []
          wm.window_set.visible
      in
      wm.window_set <- { wm.window_set with visible };
      (* set the geometry of the windows according to the layout *)
      let rects, visible_windows = List.split tiles in
      List.iter2
        (fun (rect : Rect.t) (window : window) ->
           let bw = Int32.of_int wm.config.border_width in
           let x, y, width, height =
             Int32.(
               ( of_int rect.x + bw
               , of_int rect.y + bw
               , of_int rect.w - (2l * bw)
               , of_int rect.h - (2l * bw) ))
           in
           let edges = if is_tiled window then all_edges else Edges.none in
           River_window.propose_dimensions window.proxy ~width ~height;
           River_node.set_position window.node ~x ~y;
           River_window.show window.proxy;
           River_window.set_tiled window.proxy ~edges;
           if is_tiled window then River_node.place_bottom window.node;
           window.dimensions
           <- Rect.
                { x = rect.x + Int32.to_int bw
                ; y = rect.y + Int32.to_int bw
                ; w = rect.w - (2 * Int32.to_int bw)
                ; h = rect.h - (2 * Int32.to_int bw)
                })
        rects
        visible_windows;
      (* set the border for the windows *)
      let r, g, b, a = to_river_color wm.config.normal_border_color in
      List.iter
        (fun (window : window) ->
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
      |> List.filter (fun (window : window) -> not @@ List.memq window visible_windows)
      |> List.iter (fun (window : window) -> River_window.hide window.proxy);
      (* manage focused windowed *)
      (match Window_set.focused_window wm.window_set with
       | None -> ()
       | Some window ->
         if not wm.layer_focus
         then (
           (* floating windows need to be put above others *)
           (match window.presentation with
            | Tiled -> ()
            | Floating _ -> River_node.place_top window.node);
           let r, g, b, a = to_river_color wm.config.focused_border_color in
           List.iter
             (fun (seat : seat) ->
                River_seat.focus_window seat.proxy ~window:window.proxy)
             wm.seats;
           River_window.set_borders
             window.proxy
             ~edges:all_edges
             ~width:(Int32.of_int wm.config.border_width)
             ~r
             ~g
             ~b
             ~a));
      (* set current output as default *)
      (match wm.window_set.visible with
       | [] -> ()
       | screen :: _ -> Output_layer_shell.set_default screen.output.layer_shell_proxy);
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
