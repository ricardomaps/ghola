type river_modifier = River.River_window_management_v1_client.River_seat_v1.Modifiers.t

type river_pointer_binding =
  [ `V4 ] River.River_window_management_v1_client.River_pointer_binding_v1.t

type floating_rect =
  { cx : float
  ; cy : float
  ; rw : float
  ; rh : float
  }

type presentation =
  | Tiled
  | Floating of floating_rect

type window =
  { proxy : [ `V4 ] River.River_window_management_v1_client.River_window_v1.t
  ; node : [ `V4 ] River.River_window_management_v1_client.River_node_v1.t
  ; mutable presentation : presentation
  ; mutable dimensions : Rect.t
  ; mutable title : string option
  ; mutable app_id : string option
  ; mutable identifier : string option
  ; mutable parent : window option
  ; mutable min_width : int32
  ; mutable max_width : int32
  ; mutable min_height : int32
  ; mutable max_height : int32
  }

type operation =
  | Move of
      { window : window
      ; start_x : int32
      ; start_y : int32
      ; mutable dx : int32
      ; mutable dy : int32
      }
  | Resize of
      { window : window
      ; edges : River.River_window_management_v1_client.River_window_v1.Edges.t
      ; start_rect : Rect.t
      ; mutable dx : int32
      ; mutable dy : int32
      }

type seat =
  { proxy : [ `V4 ] River.River_window_management_v1_client.River_seat_v1.t
  ; layer_shell_proxy :
      [ `V1 ] River.River_layer_shell_v1_client.River_layer_shell_seat_v1.t
  ; mutable bindings :
      [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_binding_v1.t list
  ; mutable pointer_bindings : river_pointer_binding list
  ; mutable operation : operation option
  }

type output =
  { proxy : [ `V4 ] River.River_window_management_v1_client.River_output_v1.t
  ; layer_shell_proxy :
      [ `V1 ] River.River_layer_shell_v1_client.River_layer_shell_output_v1.t
  ; mutable geometry : Rect.t
  ; mutable working_geometry : Rect.t
  }

module rec Types : sig
  type message = ..

  type window_space = (window, layout) Window_set.workspace
  and layout = Layout : (module Layout.S with type t = 'a) * 'a -> layout

  and wm =
    { config : config
    ; xkb_bindings_proxy :
        [ `V2 ] River.River_xkb_bindings_v1_client.River_xkb_bindings_v1.t
    ; window_manager_proxy :
        [ `V4 ] River.River_window_management_v1_client.River_window_manager_v1.t
    ; layer_shell_proxy : [ `V1 ] River.River_layer_shell_v1_client.River_layer_shell_v1.t
    ; queue : (unit -> unit) Queue.t
    ; env : Eio_unix.Stdenv.base
    ; mutable window_set : (window, layout, output) Window_set.t
    ; mutable seats : seat list
    ; mutable layer_focus : bool
    ; mutable hovered_window : window option
    }

  and config =
    { keys : ((river_modifier * Xkbcommon.Keysym.t) * (wm -> unit)) list
    ; pointer_bindings : ((river_modifier * int32) * (window -> wm -> unit)) list
    ; workspaces : string list
    ; layout : layout
    ; focus_follows_mouse : bool
    ; border_width : int
    ; normal_border_color : int
    ; focused_border_color : int
    ; startup_hook : wm -> unit
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
