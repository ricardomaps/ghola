type t =
  { x : int
  ; y : int
  ; w : int
  ; h : int
  }

let mirror { x; y; w; h } = { x = y; y = x; w = h; h = w }

let split_vertically n r =
  let rec go n r acc =
    if n < 2
    then List.rev @@ r :: acc
    else (
      let h = r.h / n in
      let r' = { r with y = r.y + h; h = r.h - h } in
      go (n - 1) r' ({ r with h } :: acc))
  in
  go n r []
;;

(* not the most efficient, but does it matter? at least it's short *)
let split_horizontally n r = r |> mirror |> split_vertically n |> List.map mirror

let split_horizontally_by f r =
  let w = int_of_float (Float.floor (float_of_int r.w *. f)) in
  { r with w }, { r with x = r.x + w; w = r.w - w }
;;

let split_vertically_by f r =
  let a, b = split_horizontally_by f (mirror r) in
  mirror a, mirror b
;;
