open Containers
open Common

type window_id = string
type output_name = int32

type 'layout workspace =
  { tag : string
  ; layout : 'layout
  ; stack : window_id Zipper.t option
  }

type 'layout screen =
  { workspace : 'layout workspace
  ; output : output_name
  }

type 'layout t =
  { visible : 'layout screen list
  ; hidden : 'layout workspace list
  ; floating : Floating_rect.t StringMap.t
  }

let create layout tags =
  if List.is_empty tags
  then invalid_arg "Window_set.create called with empty tag list"
  else (
    let workspaces = List.map (fun tag -> { tag; layout; stack = None }) tags in
    { visible = []; hidden = workspaces; floating = StringMap.empty })
;;

(* TODO: think about what to do when there are more screens than workspaces *)
let add_screen output window_set =
  let screen = { output; workspace = List.hd window_set.hidden } in
  { window_set with visible = screen :: window_set.visible; hidden = List.tl window_set.hidden }
;;

let remove_screen output window_set =
  let screen =
    List.find (fun screen -> Int32.equal screen.output output) window_set.visible
  in
  let visible =
    List.filter (fun screen -> not @@ Int32.equal screen.output output) window_set.visible
  in
  { window_set with visible; hidden = screen.workspace :: window_set.hidden }
;;

let current_workspace { visible; hidden } =
  match visible with
  | [] -> List.hd hidden
  | current :: _ -> current.workspace
;;

let map_current_workspace f window_set =
  match window_set.visible with
  | current :: rest ->
    { window_set with visible = { current with workspace = f current.workspace } :: rest }
  | [] ->
    let current, rest = List.hd_tl window_set.hidden in
    { window_set with hidden = f current :: rest }
;;

let update_stack f ~default =
  map_current_workspace (fun current ->
    { current with stack = Option.map_or ~default f current.stack })
;;

let map_stack f = update_stack ~default:None (fun stack -> Some (f stack))

let focused_window window_set =
  let current = current_workspace window_set in
  Option.map (fun (stack : _ Zipper.t) -> stack.focus) current.stack
;;

let workspaces window_set =
  let seen = CCList.map (fun screen -> screen.workspace) window_set.visible in
  seen @ window_set.hidden
;;

let current_tag window_set =
  let current = current_workspace window_set in
  current.tag
;;

let all_windows window_set =
  workspaces window_set
  |> List.concat_map (fun workspace ->
    Option.map_or ~default:[] Zipper.to_list workspace.stack)
;;

let focus_up window_set = map_stack Zipper.focus_up window_set
let focus_down window_set = map_stack Zipper.focus_down window_set
let swap_up window_set = map_stack Zipper.swap_up window_set
let swap_down window_set = map_stack Zipper.swap_down window_set
let focus_master window_set = map_stack Zipper.focus_master window_set
let swap_master window_set = map_stack Zipper.swap_master window_set
let shift_master window_set = map_stack Zipper.shift_master window_set

let insert_up window =
  update_stack
    ~default:(Some (Zipper.singleton window))
    (fun stack -> Some (Zipper.insert_up window stack))
;;

let lookup_workspace output window_set =
  window_set.visible
  |> List.find_opt (fun screen -> Int32.equal screen.output output)
  |> Option.map (fun screen -> screen.workspace.tag)
;;

let tag_mem tag window_set =
  workspaces window_set |> List.exists (fun workspace -> String.equal tag workspace.tag)
;;

let map_workspaces f window_set =
  let f_screen screen = { screen with workspace = f screen.workspace } in
  { window_set with
    visible = List.map f_screen window_set.visible
  ; hidden = List.map f window_set.hidden
  }
;;

let map_layouts f window_set =
  let f_workspace w = { w with layout = f w.layout } in
  let f_screen s = { s with workspace = f_workspace s.workspace } in
  { window_set with
    visible = List.map f_screen window_set.visible
  ; hidden = List.map f_workspace window_set.hidden
  }
;;

let rename_tag old_tag new_tag window_set =
  map_workspaces (fun w ->
    if String.equal w.tag old_tag then { w with tag = new_tag } else w)
;;

let find_tag window window_set =
  workspaces window_set
  |> List.find_opt (fun workspace ->
    let windows = Option.map_or ~default:[] Zipper.to_list workspace.stack in
    List.memq window windows)
  |> Option.map (fun workspace -> workspace.tag)
;;

let view tag window_set =
  match
    List.find_idx (fun scr -> String.equal scr.workspace.tag tag) window_set.visible
  with
  | Some (idx, screen) ->
    let rest = List.remove_at_idx idx window_set.visible in
    { window_set with visible = screen :: rest }
  | None ->
    (match List.find_idx (fun w -> String.equal w.tag tag) window_set.hidden with
     | None -> window_set
     | Some (idx, workspace) ->
       let rest = List.remove_at_idx idx window_set.hidden in
       (match window_set.visible with
        | [] -> { window_set with hidden = workspace :: rest }
        | current :: visible ->
          { window_set with
            visible = { current with workspace } :: visible
          ; hidden = current.workspace :: rest
          }))
;;

let greedy_view tag window_set =
  if List.exists (fun w -> String.equal w.tag tag) window_set.hidden
  then view tag window_set
  else (
    match
      List.find_idx (fun scr -> String.equal scr.workspace.tag tag) window_set.visible
    with
    | None -> window_set
    | Some (idx, screen) ->
      let current = List.hd window_set.visible in
      let current_workspace = current.workspace in
      let visible = List.remove_at_idx idx window_set.visible in
      let current = { current with workspace = screen.workspace } in
      let screen = { screen with workspace = current_workspace } in
      { window_set with visible = current :: screen :: visible })
;;

let focus_window window window_set =
  let focused ss = Option.equal String.equal (Some window) (focused_window ss) in
  let rec go ss = if focused ss then ss else go (focus_up ss) in
  if focused window_set
  then window_set
  else
    find_tag window window_set
    |> Option.map_or ~default:window_set (fun tag -> go (view tag window_set))
;;

let remove_window window window_set =
  let delete_from_workspace w =
    let open Option in
    { w with stack = w.stack >>= Zipper.filter (String.(<>) window) }
  in
  let delete_from_screen s = { s with workspace = delete_from_workspace s.workspace } in
  { visible = List.map delete_from_screen window_set.visible
  ; hidden = List.map delete_from_workspace window_set.hidden
  ; floating = StringMap.remove window window_set.floating
  }
;;

let float window float_rect window_set =
  { window_set with
    floating = StringMap.add window float_rect window_set.floating
  }

let tile window window_set =
  { window_set with
    floating = StringMap.remove window window_set.floating
  }

let on_workspace tag f window_set =
  map_workspaces (fun w -> if String.equal w.tag tag then f w else w) window_set
;;

let shift_window tag window window_set =
  match find_tag window window_set with
  | Some from when String.( <> ) from tag && tag_mem tag window_set ->
    window_set
    |> on_workspace from (fun workspace ->
      let open Option in
      { workspace with stack = workspace.stack >>= Zipper.filter (String.(<>) window) })
    |> on_workspace tag (fun workspace ->
      { workspace with
        stack =
          Option.map_or
            ~default:(Some (Zipper.singleton window))
            (fun w -> Some (Zipper.insert_up window w))
            workspace.stack
      })
  | _ -> window_set
;;

let shift tag window_set =
  match focused_window window_set with
  | None -> window_set
  | Some window -> shift_window tag window window_set
;;
