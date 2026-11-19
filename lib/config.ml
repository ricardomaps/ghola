open Common
open Containers

type ('wm, 'layout) t =
  { keys :
      ('wm, 'layout) t -> ((River_modifier.t * Xkbcommon.Keysym.t) * ('wm -> unit)) list
  ; pointer_bindings :
      ((River_modifier.t * Button.t) * (Window_set.window_id -> 'wm -> unit)) list
  ; terminal : string
  ; modifier : River_modifier.t
  ; workspaces : string list
  ; layout : 'layout
  ; focus_follows_mouse : bool
  ; border_width : int
  ; normal_border_color : Color.t
  ; focused_border_color : Color.t
  ; startup_hook : 'wm -> unit
  ; manage_hook : Window_set.window_id -> 'wm -> unit
  }
