open Containers

type ('a, 'l) workspace =
  { tag : string
  ; layout : 'l
  ; stack : 'a Zipper.t option
  }

type ('a, 'l, 'o) screen =
  { workspace : ('a, 'l) workspace
  ; output : 'o
  }

type ('a, 'l, 'o) t =
  { visible : ('a, 'l, 'o) screen list
  ; hidden : ('a, 'l) workspace list
  }

let create layout tags outputs =
  if List.is_empty tags
  then invalid_arg "Stack_set.create called with empty tag list"
  else (
    let workspaces = List.map (fun tag -> { tag; layout; stack = None }) tags in
    let seen, hidden = List.take_drop (List.length outputs) workspaces in
    let visible =
      List.map2 (fun workspace output -> { workspace; output }) seen outputs
    in
    { visible; hidden })
;;

(* TODO: think about what to do when there are more screens than workspaces *)
let add_screen output stack_set =
  let screen = { output; workspace = List.hd stack_set.hidden } in
  { visible = screen :: stack_set.visible; hidden = List.tl stack_set.hidden }
;;

let remove_screen output stack_set =
  let screen =
    List.find (fun screen -> Stdlib.( == ) screen.output output) stack_set.visible
  in
  let visible =
    List.filter (fun screen -> Stdlib.( != ) screen.output output) stack_set.visible
  in
  { visible; hidden = screen.workspace :: stack_set.hidden }
;;

let current_workspace { visible; hidden } =
  match visible with
  | [] -> List.hd hidden
  | current :: _ -> current.workspace
;;

let map_current_workspace f stack_set =
  match stack_set.visible with
  | current :: rest ->
    { stack_set with visible = { current with workspace = f current.workspace } :: rest }
  | [] ->
    let current, rest = List.hd_tl stack_set.hidden in
    { stack_set with hidden = f current :: rest }
;;

let update_stack f ~default =
  map_current_workspace (fun current ->
    { current with stack = Option.map_or ~default f current.stack })
;;

let map_stack f = update_stack ~default:None (fun stack -> Some (f stack))

let focused_window stack_set =
  let current = current_workspace stack_set in
  Option.map (fun (stack : _ Zipper.t) -> stack.focus) current.stack
;;

let workspaces stack_set =
  let seen = CCList.map (fun screen -> screen.workspace) stack_set.visible in
  seen @ stack_set.hidden
;;

let current_tag stack_set =
  let current = current_workspace stack_set in
  current.tag
;;

let all_windows stack_set =
  workspaces stack_set
  |> List.concat_map (fun workspace ->
    Option.map_or ~default:[] Zipper.to_list workspace.stack)
;;

let focus_up stack_set = map_stack Zipper.focus_up stack_set
let focus_down stack_set = map_stack Zipper.focus_down stack_set
let swap_up stack_set = map_stack Zipper.swap_up stack_set
let swap_down stack_set = map_stack Zipper.swap_down stack_set
let focus_master stack_set = map_stack Zipper.focus_master stack_set
let swap_master stack_set = map_stack Zipper.swap_master stack_set

let insert_up window =
  update_stack
    ~default:(Some (Zipper.singleton window))
    (fun stack -> Some (Zipper.insert_up window stack))
;;

let lookup_workspace output stack_set =
  stack_set.visible
  |> List.find_opt (fun screen -> screen.output Stdlib.( == ) output)
  |> Option.map (fun screen -> screen.workspace.tag)
;;

let tag_mem tag stack_set =
  workspaces stack_set |> List.exists (fun workspace -> String.equal tag workspace.tag)
;;

let map_workspaces f stack_set =
  let f_screen screen = { screen with workspace = f screen.workspace } in
  { visible = List.map f_screen stack_set.visible; hidden = List.map f stack_set.hidden }
;;

let map_layouts f stack_set =
  let f_workspace w = { w with layout = f w.layout } in
  let f_screen s = { s with workspace = f_workspace s.workspace } in
  { visible = List.map f_screen stack_set.visible
  ; hidden = List.map f_workspace stack_set.hidden
  }
;;

let rename_tag old_tag new_tag stack_set =
  map_workspaces (fun w ->
    if String.equal w.tag old_tag then { w with tag = new_tag } else w)
;;

let find_tag window stack_set =
  workspaces stack_set
  |> List.find_opt (fun workspace ->
    let windows = Option.map_or ~default:[] Zipper.to_list workspace.stack in
    List.memq window windows)
  |> Option.map (fun workspace -> workspace.tag)
;;

let view tag stack_set =
  match
    List.find_idx (fun scr -> String.equal scr.workspace.tag tag) stack_set.visible
  with
  | Some (idx, screen) ->
    let rest = List.remove_at_idx idx stack_set.visible in
    { stack_set with visible = screen :: rest }
  | None ->
    (match List.find_idx (fun w -> String.equal w.tag tag) stack_set.hidden with
     | None -> stack_set
     | Some (idx, workspace) ->
       let rest = List.remove_at_idx idx stack_set.hidden in
       (match stack_set.visible with
        | [] -> { stack_set with hidden = workspace :: rest }
        | current :: visible ->
          { visible = { current with workspace } :: visible
          ; hidden = current.workspace :: rest
          }))
;;

let greedy_view tag stack_set =
  if List.exists (fun w -> String.equal w.tag tag) stack_set.hidden
  then view tag stack_set
  else (
    match
      List.find_idx (fun scr -> String.equal scr.workspace.tag tag) stack_set.visible
    with
    | None -> stack_set
    | Some (idx, screen) ->
      let current = List.hd stack_set.visible in
      let current_workspace = current.workspace in
      let visible = List.remove_at_idx idx stack_set.visible in
      let current = { current with workspace = screen.workspace } in
      let screen = { screen with workspace = current_workspace } in
      { stack_set with visible = current :: screen :: visible })
;;

let focus_window (window : 'a) (stack_set : ('a, 'b, 'c) t) =
  let focused (ss : ('a, 'b, 'c) t) : bool =
    Option.equal Stdlib.( == ) (Some window) (focused_window ss)
  in
  let rec go ss = if focused ss then ss else go (focus_up ss) in
  if focused stack_set
  then Some stack_set
  else find_tag window stack_set |> Option.map (fun tag -> go (view tag stack_set))
;;

let remove_window window stack_set =
  let delete_from_workspace w =
    let open Option in
    { w with stack = w.stack >>= Zipper.filter (Stdlib.( != ) window) }
  in
  let delete_from_screen s = { s with workspace = delete_from_workspace s.workspace } in
  { visible = List.map delete_from_screen stack_set.visible
  ; hidden = List.map delete_from_workspace stack_set.hidden
  }
;;

let on_workspace tag f stack_set =
  map_workspaces (fun w -> if String.equal w.tag tag then f w else w) stack_set
;;

let shift_window tag window stack_set =
  match find_tag tag stack_set with
  | Some from when String.( <> ) from tag && tag_mem tag stack_set ->
    stack_set
    |> on_workspace from (fun workspace ->
      let open Option in
      { workspace with stack = workspace.stack >>= Zipper.filter (Stdlib.( != ) window) })
    |> on_workspace tag (fun workspace ->
      { workspace with
        stack =
          Option.map_or
            ~default:(Some (Zipper.singleton window))
            (fun w -> Some (Zipper.insert_up window w))
            workspace.stack
      })
  | _ -> stack_set
;;

let shift tag stack_set =
  match focused_window stack_set with
  | None -> stack_set
  | Some window -> shift_window tag window stack_set
;;
