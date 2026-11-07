open Proto
open Containers

type presentation =
  | Tiled
  | Floating of Floating_rect.t

type t =
  { proxy : [ `V4 ] River_window.t
  ; node : [ `V4 ] River_node.t
  ; mutable presentation : presentation
  ; mutable dimensions : Rect.t
  ; mutable title : string option
  ; mutable app_id : string option
  ; mutable identifier : string option
  ; mutable parent : t option
  ; mutable min_width : int32
  ; mutable max_width : int32
  ; mutable min_height : int32
  ; mutable max_height : int32
  }

let create proxy =
  { proxy
  ; presentation = Tiled
  ; node = River_window.get_node proxy (new River_node.v4)
  ; dimensions = Rect.empty
  ; title = None
  ; app_id = None
  ; identifier = None
  ; parent = None
  ; min_width = 0l
  ; max_width = 0l
  ; min_height = 0l
  ; max_height = 0l
  }
;;

let destroy window =
  River_window.destroy window.proxy;
  Wayland.Proxy.delete window.proxy
;;

let equal : t -> t -> bool = Equal.physical

let is_tiled window =
  match window.presentation with
  | Tiled -> true
  | _ -> false
;;

let is_floating window =
  match window.presentation with
  | Floating _ -> true
  | _ -> false
;;

let is_fixed_sized window =
  let open Int32 in
  window.max_height <> 0l
  && window.max_height = window.min_height
  && window.max_width <> 0l
  && window.max_width = window.min_width
;;

let has_parent window = Option.is_some window.parent

let clamp_dim ~lo ~hi v =
  let open Int32 in
  let v = if lo > 0l then max v lo else v in
  if hi > 0l then min v hi else v
;;

let clamp_size window ~w ~h =
  ( clamp_dim ~lo:window.min_width ~hi:window.max_width w
  , clamp_dim ~lo:window.min_height ~hi:window.max_height h )
;;

let place_floating window (rel : Floating_rect.t) ~(outer : Rect.t) ~border =
  let open Int32 in
  let f = Int32.to_float in
  let px v = Int32.of_float (Float.round v) in
  let iw, ih =
    clamp_size window ~w:(px (rel.rw *. f outer.w)) ~h:(px (rel.rh *. f outer.h))
  in
  let w = iw + (2l * border)
  and h = ih + (2l * border) in
  let cx = outer.x + px (rel.cx *. f outer.w) in
  let cy = outer.y + px (rel.cy *. f outer.h) in
  Rect.{ x = cx - (w / 2l); y = cy - (h / 2l); w; h }
;;
