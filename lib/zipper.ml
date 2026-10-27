type 'a t =
  { up : 'a list
  ; focus : 'a
  ; down : 'a list
  }

let singleton x = { focus = x; up = []; down = [] }
let to_list stack = List.rev stack.up @ (stack.focus :: stack.down)

let from_list = function
  | [] -> None
  | x :: xs -> Some { focus = x; up = []; down = xs }
;;

let filter f stack =
  match List.filter f (stack.focus :: stack.down) with
  | focus :: down -> Some { focus; down; up = List.filter f stack.up }
  | [] ->
    (match List.filter f stack.up with
     | focus :: up -> Some { focus; up; down = [] }
     | [] -> None)
;;

let mirror stack = { stack with up = stack.down; down = stack.up }
let mirrored f stack = mirror stack |> f |> mirror

let focus_master stack =
  match List.rev stack.up with
  | [] -> stack
  | x :: xs -> { focus = x; up = []; down = xs @ (stack.focus :: stack.down) }
;;

let focus_up stack =
  match stack.up with
  | x :: xs -> { focus = x; up = xs; down = stack.focus :: stack.down }
  | [] -> mirrored focus_master stack
;;

let focus_down stack = mirrored focus_up stack

let swap_up stack =
  match stack.up with
  | x :: xs -> { stack with up = xs; down = x :: stack.down }
  | [] -> { stack with up = List.rev stack.down; down = [] }
;;

let swap_down stack = mirrored swap_up stack
let insert_up x stack = { stack with focus = x; down = stack.focus :: stack.down }
let shift_master stack = { stack with up = []; down = List.rev stack.up @ stack.down }

let swap_master stack =
  match List.rev stack.up with
  | [] -> stack
  | x :: xs -> { stack with up = []; down = xs @ (x :: stack.down) }
;;
