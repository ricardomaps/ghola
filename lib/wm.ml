type keybinding = [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_binding_v1.t
type modifier = River.River_window_management_v1_client.River_seat_v1.Modifiers.t
type river_output = [ `V4 ] River.River_window_management_v1_client.River_output_v1.t
type river_window = [ `V4 ] River.River_window_management_v1_client.River_window_v1.t

type output =
  { proxy : river_output
  ; dimensions : Rect.t
  }

type window = { proxy : river_window }

module rec Types : sig
  type message = ..
  type workspace_id = string

  type window_space = (window, layout, workspace_id) Stack_set.workspace
  and layout = Layout : (module Layout.S with type t = 'a) * 'a -> layout

  and wm =
    { config : config
    ; mutable window_set : (window, layout, string, output) Stack_set.t
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
      -> (window, t, Types.workspace_id) Stack_set.workspace
      -> Rect.t
      -> (Rect.t * window) list * t option

    val handle_message : Types.wm -> t -> Types.message -> t option
  end
end =
  Layout

include Types

type message += ReleaseResources | Hide
