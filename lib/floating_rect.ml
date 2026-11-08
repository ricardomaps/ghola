open! Containers

type t =
  { cx : float
  ; cy : float
  ; rw : float
  ; rh : float
  }

let of_rects ~(inner : Rect.t) ~(outer : Rect.t) =
  let f = Int32.to_float in
  { cx = (f inner.x +. (f inner.w /. 2.) -. f outer.x) /. f outer.w
  ; cy = (f inner.y +. (f inner.h /. 2.) -. f outer.y) /. f outer.h
  ; rw = f inner.w /. f outer.w
  ; rh = f inner.h /. f outer.h
  }
;;

let default = { cx = 0.5; cy = 0.5; rw = 0.67; rh = 0.67 }
