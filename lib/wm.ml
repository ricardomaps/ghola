include Types.Wm
open Proto
open Containers
open Int32

let spawn cmd wm =
  Eio.Process.run
    wm.env#process_mgr
    [ "sh"; "-c"; {|setsid -f sh -c "$1" </dev/null >/dev/null 2>&1|}; "spawn"; cmd ]
;;

let defer wm f = Queue.add f wm.queue

let run_queue wm =
  Queue.iter (fun action -> action ()) wm.queue;
  Queue.clear wm.queue
;;

let windows f wm = wm.window_set <- f wm.window_set

let kill_focused_window wm =
  match Window_set.focused_window wm.window_set with
  | None -> ()
  | Some window -> defer wm (fun () -> River_window.close window.proxy)
;;

let cancel_operation (seat : Seat.t) wm =
  (match seat.operation with
   | None -> ()
   | Some (Resize { window; _ }) ->
     defer wm (fun () ->
       River_seat.op_end seat.proxy;
       River_window.inform_resize_end window.proxy)
   | Some (Move _) -> defer wm (fun () -> River_seat.op_end seat.proxy));
  seat.operation <- None
;;

let add_seat seat wm = wm.seats <- wm.seats @ [ seat ]

let screen_at x y (window_set : _ Window_set.t) =
  window_set.visible
  |> List.find_opt (fun (screen : _ Window_set.screen) ->
    Rect.is_point_within x y screen.output.geometry)
;;

let float (window : Window.t) wm =
  match wm.window_set.visible with
  | [] -> ()
  | current :: _ ->
    let d = window.dimensions in
    let screen =
      Option.value
        ~default:current
        (screen_at (d.x + (d.w / 2l)) (d.y + (d.h / 2l)) wm.window_set)
    in
    wm.window_set <- Window_set.shift_window screen.workspace.tag window wm.window_set;
    window.presentation
    <- Floating (Floating_rect.of_rects ~inner:d ~outer:screen.output.geometry)
;;

let tile_window wm =
  Window_set.focused_window wm.window_set
  |> Option.iter (fun (w : Window.t) -> w.presentation <- Tiled)
;;

let float_window wm =
  Window_set.focused_window wm.window_set
  |> Option.iter (fun (w : Window.t) -> float w wm)
;;

let send_message message wm =
  windows
    (Window_set.map_current_workspace (fun current ->
       let (Layout.Layout ((module L), layout)) = current.layout in
       match L.handle_message wm layout message with
       | None -> current
       | Some layout -> { current with layout = Layout ((module L), layout) }))
    wm
;;

let broadcast_message message wm =
  Window_set.map_workspaces
    (fun workspace ->
       let (Layout.Layout ((module L), layout)) = workspace.layout in
       match L.handle_message wm layout message with
       | None -> workspace
       | Some layout -> { workspace with layout = Layout ((module L), layout) })
    wm.window_set
;;

let set_layout layout wm =
  wm.window_set
  <- Window_set.map_current_workspace
       (fun current ->
          let (Layout.Layout ((module L), l)) = current.layout in
          ignore @@ L.handle_message wm l Message.ReleaseResources;
          { current with layout })
       wm.window_set
;;

let seat wm = List.hd wm.seats
let exit_wm wm = River_window_manager.exit_session wm.window_manager_proxy

let resize_window window wm =
  let seat = seat wm in
  float window wm;
  let operation =
    Operation.Resize
      { window
      ; edges = Edges.(union bottom right)
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
    Operation.Move
      { window
      ; start_x = window.dimensions.x
      ; start_y = window.dimensions.y
      ; dx = 0l
      ; dy = 0l
      }
  in
  seat.operation <- Some operation;
  Queue.add (fun () -> River_seat.op_start_pointer seat.proxy) wm.queue
;;

let parent_workspace (window : Window.t) wm =
  Option.bind window.parent (fun parent -> Window_set.find_tag parent wm.window_set)
;;

let init_window (window : Window.t) wm =
  let initial_floating = Floating_rect.{ cx = 0.5; cy = 0.5; rw = 0.5; rh = 0.5 } in
  River_window.use_ssd window.proxy;
  (* send a transient window to its parent's workspace *)
  parent_workspace window wm
  |> Option.iter (fun tag -> windows (Window_set.shift_window tag window) wm);
  if Window.is_fixed_sized window || Window.has_parent window
  then window.presentation <- Floating initial_floating
;;

let resize_delta ~neg ~pos edges d =
  if Edges.contains neg edges then -d else if Edges.contains pos edges then d else 0l
;;

let apply_resize wm (seat : Seat.t) =
  match seat.operation with
  | Some (Resize { window; dx; dy; start_rect; edges }) ->
    let w = start_rect.w + resize_delta ~neg:Edges.left ~pos:Edges.right edges dx in
    let h = start_rect.h + resize_delta ~neg:Edges.top ~pos:Edges.bottom edges dy in
    window.dimensions <- { window.dimensions with w; h };
    float window wm
  | Some (Move _) | None -> ()
;;

let apply_resizes wm = List.iter (apply_resize wm) wm.seats

let floating_placements wm (screen : _ Window_set.screen) =
  Option.map_or ~default:[] Zipper.to_list screen.workspace.stack
  |> List.filter_map (fun (w : Window.t) ->
    match w.presentation with
    | Tiled -> None
    | Floating rel ->
      let rect =
        Window.place_floating
          w
          rel
          ~outer:screen.output.geometry
          ~border:(Int32.of_int wm.config.border_width)
      in
      Some (rect, w))
;;

let layout_screen wm (screen : _ Window_set.screen) =
  let open Layout in
  let (Layout ((module L), state)) = screen.workspace.layout in
  let tiled_stack =
    Option.bind screen.workspace.stack (Zipper.filter Window.is_tiled)
  in
  let tiled_workspace = { screen.workspace with layout = state; stack = tiled_stack } in
  let tiles, new_state =
    L.run_layout wm tiled_workspace screen.output.working_geometry
  in
  let floats = floating_placements wm screen in
  let screen =
    match new_state with
    | None -> screen
    | Some s ->
      { screen with workspace = { screen.workspace with layout = Layout ((module L), s) } }
  in
  floats @ tiles, screen
;;

let arrange wm =
  let placed, visible =
    List.fold_map
      (fun acc screen ->
         let placed, screen = layout_screen wm screen in
         placed @ acc, screen)
      []
      wm.window_set.visible
  in
  wm.window_set <- { wm.window_set with visible };
  placed
;;

let border_color wm ~focused (window : Window.t) =
  let is_focused =
    not wm.layer_focus && Option.equal Window.equal focused (Some window)
  in
    if is_focused then wm.config.focused_border_color else wm.config.normal_border_color
;;

let place wm ~focused ((rect : Rect.t), (window : Window.t)) =
  let bw = wm.config.border_width in
  let inner = Rect.shrink (Int32.of_int bw) rect in
  let edges = if Window.is_tiled window then Edges.all else Edges.none in
  River_window.propose_dimensions
    window.proxy
    ~width:inner.w
    ~height:inner.h;
  River_node.set_position window.node ~x:(inner.x) ~y:inner.y;
  River_window.show window.proxy;
  River_window.set_tiled window.proxy ~edges;
  if Window.is_tiled window then River_node.place_bottom window.node;
  let Color.{ r; g; b; a }= border_color wm ~focused window in
  River_window.set_borders
    window.proxy
    ~edges:Edges.all
    ~width:(Int32.of_int bw)
    ~r ~g ~b ~a;
  window.dimensions <- inner
;;

let hide_others wm placed =
  let visible = List.map snd placed in
  Window_set.all_windows wm.window_set
  |> List.filter (fun w -> not @@ List.memq w visible)
  |> List.iter (fun (w : Window.t) -> River_window.hide w.proxy)
;;

let focus wm =
  match Window_set.focused_window wm.window_set with
  | Some window when not wm.layer_focus ->
    (match window.presentation with
     | Floating _ -> River_node.place_top window.node
     | Tiled -> ());
    List.iter
      (fun (seat : Seat.t) -> River_seat.focus_window seat.proxy ~window:window.proxy)
      wm.seats
  | _ -> ()
;;

let set_default_output wm =
  match wm.window_set.visible with
  | [] -> ()
  | screen :: _ -> River_layer_shell_output.set_default screen.output.layer_shell_proxy
;;

let manage_start wm =
  run_queue wm;
  apply_resizes wm;
  let placed = arrange wm in
  let focused = Window_set.focused_window wm.window_set in
  List.iter (place wm ~focused) placed;
  hide_others wm placed;
  focus wm;
  set_default_output wm
;;

let render_start wm =
  List.filter_map (fun (s : Seat.t) -> s.operation) wm.seats
  |> List.iter (function
    | Operation.Move { window; start_x; start_y; dx; dy } ->
      let x = start_x + dx in
      let y = start_y + dy in
      window.dimensions
      <- { window.dimensions with x; y };
      float window wm;
      River_node.set_position window.node ~x ~y
    | Operation.Resize { window; start_rect = { x; y; w; h }; edges; _ } ->
      let x = if Edges.contains Edges.left edges then x + w - window.dimensions.w else x in
      let y = if Edges.contains Edges.top edges then y + h - window.dimensions.h else y in
      window.dimensions <- { window.dimensions with x; y };
      float window wm;
      River_node.set_position window.node ~x ~y)
;;
