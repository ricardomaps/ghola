type rect =
  { x : int32
  ; y : int32
  ; w : int32
  ; h : int32
  }

type message = ..

module type S = sig
  type t
  type wm
  type workspace

  val description : t -> string
  val do_layout : wm -> workspace -> t -> (rect * 'a) list * t option
  val handle_message : wm -> message -> t -> t option
end
