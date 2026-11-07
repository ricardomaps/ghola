type t =
  | Move of
      { window : Window.t
      ; start_x : int32
      ; start_y : int32
      ; mutable dx : int32
      ; mutable dy : int32
      }
  | Resize of
      { window : Window.t
      ; edges : Edges.t
      ; start_rect : Rect.t
      ; mutable dx : int32
      ; mutable dy : int32
      }

let window = function
  | Move { window; _ } | Resize { window; _ } -> window
;;

let set_delta ~dx ~dy = function
  | Resize data ->
    data.dx <- dx;
    data.dy <- dy
  | Move data ->
    data.dx <- dx;
    data.dy <- dy
;;
