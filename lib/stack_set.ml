type ('a, 'l, 'i) workspace =
  { tag : 'i
  ; layout : 'l
  ; stack : 'a Stack.t option
  }

type ('a, 'l, 'i, 'o) screen =
  { workspace : ('a, 'l, 'i) workspace
  ; output : 'o
  }

type ('a, 'l, 'i, 'o) t =
  { current : ('a, 'l, 'i, 'o) screen
  ; visible : ('a, 'l, 'i, 'o) screen list
  ; hidden : ('a, 'l, 'i) workspace list
  }

module Workspace = struct
  let windows workspace = Option.fold ~none:[] ~some:Stack.to_list workspace.stack

  let delete window workspace =
    { workspace with stack = Option.bind workspace.stack (Stack.filter (( != ) window)) }
  ;;

  let insert_up window workspace =
    { workspace with
      stack =
        Option.fold
          ~none:(Some (Stack.singleton window))
          ~some:(fun w -> Some (Stack.insert_up window w))
          workspace.stack
    }
  ;;
end

let create layout tags outputs =
  let n = List.length outputs in
  if n = 0 || n < List.length tags
  then invalid_arg "non-positive argument to Stack_set.create"
  else (
    let workspaces = List.map (fun tag -> { layout; tag; stack = None }) tags in
    let seen, hidden = List.(take n workspaces, drop n workspaces) in
    let screens =
      List.map2 (fun workspace output -> { workspace; output }) seen outputs
    in
    { current = List.hd screens; visible = List.tl screens; hidden })
;;

let fold_stack stack_set ~none ~some =
  Option.fold ~none ~some stack_set.current.workspace.stack
;;

let update_stack stack_set ~none ~some =
  let stack = fold_stack ~none ~some stack_set in
  let workspace = { stack_set.current.workspace with stack } in
  let current = { stack_set.current with workspace } in
  { stack_set with current }
;;

let map_stack f = update_stack ~none:None ~some:(fun stack -> Some (f stack))
let peek = fold_stack ~none:None ~some:(fun stack -> Some stack.focus)
let screens stack_set = stack_set.current :: stack_set.visible

let workspaces stack_set =
  let hidden = stack_set.hidden in
  let current = stack_set.current.workspace in
  let visible = List.map (fun s -> s.workspace) stack_set.visible in
  (current :: visible) @ hidden
;;

let current_tag stack_set = stack_set.current.workspace.tag
let current_windows = fold_stack ~none:[] ~some:Stack.to_list
let all_windows stack_set = workspaces stack_set |> List.concat_map Workspace.windows
let focus_up stack_set = map_stack Stack.focus_up stack_set
let focus_down stack_set = map_stack Stack.focus_down stack_set
let swap_up stack_set = map_stack Stack.swap_up stack_set
let swap_down stack_set = map_stack Stack.swap_down stack_set

let insert_up window stack_set =
  update_stack
    ~none:(Some (Stack.singleton window))
    ~some:(fun stack -> Some (Stack.insert_up window stack))
;;

let lookup_workspace output stack_set =
  screens stack_set
  |> List.find_opt (fun scr -> scr.output = output)
  |> Option.map (fun scr -> scr.workspace.tag)
;;

let tag_mem tag stack_set =
  workspaces stack_set |> List.exists (fun workspace -> tag = workspace.tag)
;;

let window_mem window stack_set = all_windows stack_set |> List.memq window

let map_workspaces f stack_set =
  let aux screen = { screen with workspace = f screen.workspace } in
  { current = aux stack_set.current
  ; visible = List.map aux stack_set.visible
  ; hidden = List.map f stack_set.hidden
  }
;;

let map_layouts f stack_set =
  let f_workspace w = { w with layout = f w.layout } in
  let f_screen s = { s with workspace = f_workspace s.workspace } in
  { current = f_screen stack_set.current
  ; visible = List.map f_screen stack_set.visible
  ; hidden = List.map f_workspace stack_set.hidden
  }
;;

let rename_tag old_tag new_tag stack_set =
  map_workspaces (fun w -> if w.tag = old_tag then { w with tag = new_tag } else w)
;;

let find_tag window stack_set =
  workspaces stack_set
  |> List.find_opt (fun workspace -> List.memq window (Workspace.windows workspace))
  |> Option.map (fun workspace -> workspace.tag)
;;

(* if a tag is used by multiple workspaces this also returns stack_set unaltered  *)
let view tag stack_set =
  if stack_set.current.workspace.tag = tag
  then stack_set
  else (
    match List.partition (fun scr -> scr.workspace.tag = tag) stack_set.visible with
    | [ screen ], others ->
      { stack_set with current = screen; visible = stack_set.current :: others }
    | _ ->
      (match List.partition (fun w -> w.tag = tag) stack_set.hidden with
       | [ workspace ], others ->
         { stack_set with
           current = { stack_set.current with workspace }
         ; hidden = stack_set.current.workspace :: others
         }
       | _ -> stack_set))
;;

let focus_window window stack_set =
  let focused ss = Option.equal ( == ) (Some window) (peek ss) in
  let rec go ss = if focused ss then ss else go (focus_up ss) in
  if focused stack_set
  then Some stack_set
  else Option.map (fun tag -> go (view tag stack_set)) (find_tag window stack_set)
;;

let delete window stack_set =
  let delete_from_screen s = { s with workspace = Workspace.delete window s.workspace } in
  { current = delete_from_screen stack_set.current
  ; visible = List.map delete_from_screen stack_set.visible
  ; hidden = List.map (Workspace.delete window) stack_set.hidden
  }
;;

let on_workspace tag f stack_set =
  map_workspaces (fun w -> if w.tag = tag then f w else w) stack_set
;;

let shift_window tag window stack_set =
  match find_tag tag stack_set with
  | Some from when from <> tag && tag_mem tag stack_set ->
    stack_set
    |> on_workspace from (Workspace.delete window)
    |> on_workspace tag (Workspace.insert_up window)
  | _ -> stack_set
;;

let shift tag stack_set =
  match peek stack_set with
  | None -> stack_set
  | Some window -> shift_window tag window stack_set
;;
