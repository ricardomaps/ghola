open Containers
open Int32

type t =
  { x : int32
  ; y : int32
  ; w : int32
  ; h : int32
  }

let empty = { x = 0l; y = 0l; w = 0l; h = 0l }
let mirror { x; y; w; h } = { x = y; y = x; w = h; h = w }

let split_vertically n r =
  let rec go n r acc =
    if Int.(n < 2)
    then List.rev @@ (r :: acc)
    else (
      let h = r.h / Int32.of_int n in
      let r' = { r with y = r.y + h; h = r.h - h } in
      go Int.(n - 1) r' ({ r with h } :: acc))
  in
  go n r []
;;

(* not the most efficient, but does it matter? at least it's short *)
let split_horizontally n r = r |> mirror |> split_vertically n |> List.map mirror

let split_horizontally_by f r =
  let w = Int32.of_float (Int32.to_float r.w *. f) in
  { r with w }, { r with x = r.x + w; w = r.w - w }
;;

let split_vertically_by f r =
  let a, b = split_horizontally_by f (mirror r) in
  mirror a, mirror b
;;

let is_point_within x y rect =
  x >= rect.x && y >= rect.y && x <= rect.x + rect.w && y <= rect.y + rect.h
;;

let shrink by { x; y; w; h } =
  { x = x + by
  ; y = y + by
  ; w = w - 2l * by
  ; h = h - 2l * by
  }
