type keybinding = [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_binding_v1.t
type river_seat = [ `V4 ] River.River_window_management_v1_client.River_seat_v1.t
type modifier = River.River_window_management_v1_client.River_seat_v1.Modifiers.t
type river_output = [ `V4 ] River.River_window_management_v1_client.River_output_v1.t
type river_window = [ `V4 ] River.River_window_management_v1_client.River_window_v1.t

type output =
  { proxy : river_output
  ; mutable dimensions : Rect.t
  }

type window =
  { proxy : river_window
  ; mutable dimensions : Rect.t
  ; mutable title : string option
  ; mutable app_id : string option
  ; mutable identifier : string option
  ; mutable parent : window option
  }

type seat =
  { proxy : river_seat
  ; mutable bindings : keybinding list
  }

module rec Types : sig
  type message = ..

  type window_space = (window, layout) Stack_set.workspace
  and layout = Layout : (module Layout.S with type t = 'a) * 'a -> layout

  and wm =
    { config : config
    ; mutable window_set : (window, layout, output) Stack_set.t
    }

  and config =
    { keys : config -> (keybinding, wm -> unit) Hashtbl.t
    ; modifier : modifier
    ; workspaces : string list
    }
end =
  Types

and Layout : sig
  module type S = sig
    type t

    val description : t -> string

    val run_layout
      :  Types.wm
      -> (window, t) Stack_set.workspace
      -> Rect.t
      -> (Rect.t * window) list * t option

    val handle_message : Types.wm -> t -> Types.message -> t option
  end
end =
  Layout

include Types

type message += ReleaseResources | Hide
