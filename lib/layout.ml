include Types.Layout

let pure_layout f 
  :  Types.Wm.t
  -> (Window.t, t) Window_set.workspace
  -> Rect.t
  -> (Rect.t * Window.t) list * t option
  = fun _ workspace rect -> 
  match workspace.stack with
  | None -> ([], None)
  | Some stack -> (f workspace.layout rect stack, None)
