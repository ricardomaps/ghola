open! Common
open! Containers
open Int32

type t =
  { config : (t, layout) Config.t
  ; xkb_bindings_proxy : [ `V2 ] River_xkb_bindings.t
  ; window_manager_proxy : [ `V4 ] River_window_manager.t
  ; layer_shell_proxy : [ `V1 ] River_layer_shell.t
  ; queue : (unit -> unit) Queue.t
  ; env : Eio_unix.Stdenv.base
  ; mutable window_set : layout Window_set.t
  ; mutable seats : Seat.t list
  ; mutable windows : Window.t StringMap.t
  ; mutable outputs : Output.t Int32Map.t
  ; mutable layer_focus : bool
  ; mutable hovered_window : Window_set.window_id option
  }

and layout = Layout : (module Layout.S with type t = 'a and type wm = t) * 'a -> layout

module type GholaLayout = sig
  include Layout.S with type wm = t
end

let get_window window_id wm =
  StringMap.get window_id wm.windows
  |> Option.get_exn_or ("No window under identifier " ^ window_id)
;;

let get_output output_name wm =
  Int32Map.get output_name wm.outputs
  |> Option.get_exn_or ("No output under name " ^ Int32.to_string output_name)
;;

let add_window id window wm = wm.windows <- StringMap.add id window wm.windows
let add_output name output wm = wm.outputs <- Int32Map.add name output wm.outputs
let is_floating wm window_id = StringMap.mem window_id wm.window_set.floating
let is_tiled wm = Fun.negate (is_floating wm)

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
  | Some window_id ->
    let window = get_window window_id wm in
    defer wm (fun () -> River_window.close window.proxy)
;;

let cancel_operation (seat : Seat.t) wm =
  (match seat.operation with
   | None -> ()
   | Some (Resize { window; _ }) ->
     let window = Option.get_exn_or "No such window" (StringMap.get window wm.windows) in
     defer wm (fun () ->
       River_seat.op_end seat.proxy;
       River_window.inform_resize_end window.proxy)
   | Some (Move _) -> defer wm (fun () -> River_seat.op_end seat.proxy));
  seat.operation <- None
;;

let add_seat seat wm = wm.seats <- wm.seats @ [ seat ]

let screen_at x y wm =
  wm.window_set.visible
  |> List.find_opt (fun (screen : _ Window_set.screen) ->
    let output =
      Option.get_exn_or "No such output" (Int32Map.get screen.output wm.outputs)
    in
    Rect.is_point_within x y output.geometry)
;;

let float window_id wm =
  match wm.window_set.visible with
  | [] -> ()
  | current :: _ ->
    let window = get_window window_id wm in
    let d = window.dimensions in
    let screen =
      Option.value ~default:current (screen_at (d.x + (d.w / 2l)) (d.y + (d.h / 2l)) wm)
    in
    let output = get_output screen.output wm in
    let float_rect = Floating_rect.of_rects ~inner:d ~outer:output.geometry in
    windows
      (fun ws ->
         Window_set.shift_window screen.workspace.tag window_id ws
         |> Window_set.float window_id ~rel:float_rect)
      wm
;;

let tile_window wm =
  Window_set.focused_window wm.window_set
  |> Option.iter (fun w -> windows (Window_set.tile w) wm)
;;

let float_window wm =
  Window_set.focused_window wm.window_set |> Option.iter (fun w -> float w wm)
;;

let send_message message wm =
  windows
    (Window_set.map_current_workspace (fun current ->
       let (Layout ((module L), layout)) = current.layout in
       match L.handle_message wm layout message with
       | None -> current
       | Some layout -> { current with layout = Layout ((module L), layout) }))
    wm
;;

let broadcast_message message wm =
  Window_set.map_workspaces
    (fun workspace ->
       let (Layout ((module L), layout)) = workspace.layout in
       match L.handle_message wm layout message with
       | None -> workspace
       | Some layout -> { workspace with layout = Layout ((module L), layout) })
    wm.window_set
;;

let set_layout layout wm =
  wm.window_set
  <- Window_set.map_current_workspace
       (fun current ->
          let (Layout ((module L), l)) = current.layout in
          ignore @@ L.handle_message wm l Message.ReleaseResources;
          { current with layout })
       wm.window_set
;;

let seat wm = List.hd wm.seats
let exit_wm wm = River_window_manager.exit_session wm.window_manager_proxy

let resize_window window_id wm =
  let window = get_window window_id wm in
  let operation =
    Operation.Resize
      { window = window_id
      ; edges = Edges.(union bottom right)
      ; dx = 0l
      ; dy = 0l
      ; start_rect = window.dimensions
      }
  in
  let seat = seat wm in
  seat.operation <- Some operation;
  float window_id wm;
  Queue.add
    (fun () ->
       River_seat.op_start_pointer seat.proxy;
       River_window.inform_resize_start window.proxy)
    wm.queue
;;

let move_window window_id wm =
  let seat = seat wm in
  let window = get_window window_id wm in
  float window_id wm;
  let operation =
    Operation.Move
      { window = window_id
      ; start_x = window.dimensions.x
      ; start_y = window.dimensions.y
      ; dx = 0l
      ; dy = 0l
      }
  in
  seat.operation <- Some operation;
  Queue.add (fun () -> River_seat.op_start_pointer seat.proxy) wm.queue
;;

let resize_delta ~neg ~pos edges d =
  if Edges.contains neg edges then -d else if Edges.contains pos edges then d else 0l
;;

let apply_resize wm (seat : Seat.t) =
  match seat.operation with
  | Some (Resize { window = window_id; dx; dy; start_rect; edges }) ->
    let w = start_rect.w + resize_delta ~neg:Edges.left ~pos:Edges.right edges dx in
    let h = start_rect.h + resize_delta ~neg:Edges.top ~pos:Edges.bottom edges dy in
    let window = get_window window_id wm in
    window.dimensions <- { window.dimensions with w; h };
    float window_id wm
  | Some (Move _) | None -> ()
;;

let apply_resizes wm = List.iter (apply_resize wm) wm.seats

let floating_placements wm (screen : _ Window_set.screen) =
  let output = get_output screen.output wm in
  Option.map_or ~default:[] Zipper.to_list screen.workspace.stack
  |> List.filter_map (fun w ->
    StringMap.get w wm.window_set.floating |> Option.map (fun float_rect -> float_rect, w))
  |> List.map (fun (float_rect, w) ->
    let window = get_window w wm in
    let rect =
      Window.place_floating
        window
        float_rect
        ~outer:output.geometry
        ~border:(Int32.of_int wm.config.border_width)
    in
    rect, w)
;;

let layout_screen wm (screen : _ Window_set.screen) =
  let output = get_output screen.output wm in
  let (Layout ((module L), state)) = screen.workspace.layout in
  let tiled_stack =
    Option.bind screen.workspace.stack (Zipper.filter (is_tiled wm))
  in
  let tiled_workspace = { screen.workspace with layout = state; stack = tiled_stack } in
  let tiles, new_state = L.run_layout wm tiled_workspace output.working_geometry in
  let floats = floating_placements wm screen in
  let screen =
    match new_state with
    | None -> screen
    | Some s ->
      { screen with
        workspace = { screen.workspace with layout = Layout ((module L), s) }
      }
  in
  (floats @ tiles), screen
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

let border_color wm ~focused window_id =
  let is_focused =
    (not wm.layer_focus) && Option.equal String.equal focused (Some window_id)
  in
  if is_focused then wm.config.focused_border_color else wm.config.normal_border_color
;;

let place wm ~focused ((rect : Rect.t), window_id) =
  let window = get_window window_id wm in
  let bw = wm.config.border_width in
  let inner = Rect.shrink (Int32.of_int bw) rect in
  let edges = if is_tiled wm window_id then Edges.all else Edges.none in
  River_window.propose_dimensions window.proxy ~width:inner.w ~height:inner.h;
  River_node.set_position window.node ~x:inner.x ~y:inner.y;
  River_window.show window.proxy;
  River_window.set_tiled window.proxy ~edges;
  if is_tiled wm window_id then River_node.place_bottom window.node;
  let Color.{ r; g; b; a } = border_color wm ~focused window_id in
  River_window.set_borders
    window.proxy
    ~edges:Edges.all
    ~width:(Int32.of_int bw)
    ~r
    ~g
    ~b
    ~a;
  window.dimensions <- inner
;;

let hide_others wm placed =
  let visible = List.map snd placed in
  Window_set.all_windows wm.window_set
  |> List.filter (fun w -> not @@ List.mem ~eq:String.equal w visible)
  |> List.map (fun w -> get_window w wm)
  |> List.iter (fun (w : Window.t) -> River_window.hide w.proxy)
;;

let focus wm =
  match Window_set.focused_window wm.window_set with
  | Some window_id when not wm.layer_focus ->
    let window = get_window window_id wm in
    if is_floating wm window_id then River_node.place_top window.node;
    List.iter
      (fun (seat : Seat.t) -> River_seat.focus_window seat.proxy ~window:window.proxy)
      wm.seats
  | _ -> ()
;;

let set_default_output wm =
  match wm.window_set.visible with
  | [] -> ()
  | screen :: _ ->
    let output = get_output screen.output wm in
    River_layer_shell_output.set_default output.layer_shell_proxy
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
    | Operation.Move { window = window_id; start_x; start_y; dx; dy } ->
      let x = start_x + dx in
      let y = start_y + dy in
      let window = get_window window_id wm in
      window.dimensions <- { window.dimensions with x; y };
      float window_id wm;
      River_node.set_position window.node ~x ~y
    | Operation.Resize { window = window_id; start_rect = { x; y; w; h }; edges; _ } ->
      let window = get_window window_id wm in
      let x =
        if Edges.contains Edges.left edges then x + w - window.dimensions.w else x
      in
      let y = if Edges.contains Edges.top edges then y + h - window.dimensions.h else y in
      window.dimensions <- { window.dimensions with x; y };
      float window_id wm;
      River_node.set_position window.node ~x ~y)
;;
