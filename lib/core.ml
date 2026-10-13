open Layout

type keybinding = [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_binding_v1.t
type modifier = River.River_window_management_v1_client.River_seat_v1.Modifiers.t
type river_output = [ `V4 ] River.River_window_management_v1_client.River_output_v1.t
type river_window = [ `V4 ] River.River_window_management_v1_client.River_window_v1.t

type rect =
  { x : int32
  ; y : int32
  ; w : int32
  ; h : int32
  }

type output =
  { proxy : river_output
  ; dimensions : rect
  }

type window = { proxy : river_window }

type message = ..

module rec Types : sig
  type workspace_id = string

  type window_space =
    (window, layout, workspace_id) Stack_set.workspace

  and layout =
    | Layout :
        (module Layout.S with type t = 'a) * 'a
        -> layout

  and wm =
    { config : config 
    ; mutable window_set :
        (window, layout, string, output) Stack_set.t
    }

and config =
  { keys : config -> (keybinding, wm -> unit) Hashtbl.t
  ; modifier : modifier
  ; workspaces : string list
  }
end = Types
and Layout : sig
  module type S = sig
    type t

    val description : t -> string

    val run_layout :
      Types.wm -> (window, t, Types.workspace_id) Stack_set.workspace -> rect ->
      (rect * window) list * t option

    val handle_message :
      Types.wm -> t -> message -> t option
  end
end = Layout

include Types

let make_layout_instance (type a) (module L : Layout.S with type t = a) (l : a) =
   Layout ((module L), l)

type message += Release_resources
