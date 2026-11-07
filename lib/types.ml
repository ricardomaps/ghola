open Proto

module rec Wm : sig
  type t =
    { config : Config.t
    ; xkb_bindings_proxy : [ `V2 ] River_xkb_bindings.t
    ; window_manager_proxy : [ `V4 ] River_window_manager.t
    ; layer_shell_proxy : [ `V1 ] River_layer_shell.t
    ; queue : (unit -> unit) Queue.t
    ; env : Eio_unix.Stdenv.base
    ; mutable window_set : (Window.t, Layout.t, Output.t) Window_set.t
    ; mutable seats : Seat.t list
    ; mutable layer_focus : bool
    ; mutable hovered_window : Window.t option
    }
end =
  Wm

and Config : sig
  type t =
    { keys : ((River_modifier.t * Xkbcommon.Keysym.t) * (Wm.t -> unit)) list
    ; pointer_bindings : ((River_modifier.t * int32) * (Window.t -> Wm.t -> unit)) list
    ; workspaces : string list
    ; layout : Layout.t
    ; focus_follows_mouse : bool
    ; border_width : int
    ; normal_border_color : Color.t
    ; focused_border_color : Color.t
    ; startup_hook : Wm.t -> unit
    ; manage_hook : Wm.t -> Window.t -> unit
    }
end =
  Config

and Layout : sig
  type t = Layout : (module Layout.S with type t = 'a) * 'a -> t

  module type S = sig
    type t

    val description : t -> string

    val run_layout
      :  Wm.t
      -> (Window.t, t) Window_set.workspace
      -> Rect.t
      -> (Rect.t * Window.t) list * t option

    val handle_message : Wm.t -> t -> Message.t -> t option
  end
end =
  Layout
