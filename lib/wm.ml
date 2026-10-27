type river_xkb_bindings =
  [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_bindings_v1.t

type river_window_manager =
  [ `V4 ] River.River_window_management_v1_client.River_window_manager_v1.t

type river_xkb_binding = [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_binding_v1.t
type river_seat = [ `V4 ] River.River_window_management_v1_client.River_seat_v1.t
type river_modifier = River.River_window_management_v1_client.River_seat_v1.Modifiers.t
type river_output = [ `V4 ] River.River_window_management_v1_client.River_output_v1.t
type river_window = [ `V4 ] River.River_window_management_v1_client.River_window_v1.t
type river_node = [ `V4 ] River.River_window_management_v1_client.River_node_v1.t

type seat =
  { proxy : river_seat
  ; mutable bindings : river_xkb_binding list
  }

type output =
  { proxy : river_output
  ; mutable dimensions : Rect.t
  }

type window =
  { proxy : river_window
  ; node : river_node
  ; mutable dimensions : Rect.t
  ; mutable title : string option
  ; mutable app_id : string option
  ; mutable identifier : string option
  ; mutable parent : window option
  }

module rec Types : sig
  type message = ..

  type window_space = (window, layout) Window_set.workspace
  and layout = Layout : (module Layout.S with type t = 'a) * 'a -> layout

  and wm =
    { config : config
    ; xkb_bindings_proxy : river_xkb_bindings
    ; window_manager_proxy : river_window_manager
    ; queue : (unit -> unit) Queue.t
    ; env : Eio_unix.Stdenv.base
    ; mutable window_set : (window, layout, output) Window_set.t
    ; mutable seats : seat list
    }

  and config =
    { keys : (river_modifier * Xkbcommon.Keysym.t, wm -> unit) Hashtbl.t
    ; workspaces : string list
    ; layout : layout
    ; focus_follows_mouse : bool
    }
end =
  Types

and Layout : sig
  module type S = sig
    type t

    val description : t -> string

    val run_layout
      :  Types.wm
      -> (window, t) Window_set.workspace
      -> Rect.t
      -> (Rect.t * window) list * t option

    val handle_message : Types.wm -> t -> Types.message -> t option
  end
end =
  Layout

include Types

type message += ReleaseResources | Hide
