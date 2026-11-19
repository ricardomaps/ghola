module type S = sig
  type t
  type wm = Wm.t

  val description : t -> string

  val run_layout
    :  wm
    -> t Window_set.workspace
    -> Rect.t
    -> (Rect.t * Window_set.window_id) list * t option

  val handle_message : wm -> t -> Message.t -> t option
  val yojson_of_t : t -> Yojson.Safe.t
  val t_of_yojson : Yojson.Safe.t -> t
end
