include Types.Layout

let pure_layout f
  :  Types.Wm.t
  -> t Window_set.workspace
  -> Rect.t
  -> (Rect.t * 'a) list * t option
  =
  fun _ workspace rect ->
  match workspace.stack with
  | None -> [], None
  | Some stack -> f workspace.layout rect stack, None
;;
