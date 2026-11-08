open Containers

type 'a t = Window.t -> Wm.t -> 'a

let return x = fun _ _ -> x
let map f q = fun window wm -> f (q window wm)
let map2 f a b = fun w wm -> f (a w wm) (b w wm)
let bind q f = fun w wm -> f (q w wm) w wm
let both a b = map2 (fun x y -> x, y) a b
let join qq = fun w wm -> qq w wm w wm
let app_id : string t = fun window _ -> Option.get_or ~default:"" window.app_id
let title : string t = fun window _ -> Option.get_or ~default:"" window.title
let is query s = map (String.equal s) query
let contains query sub = map (fun v -> String.mem ~sub v) query
let not query = map not query
let all queries = fun window wm -> List.for_all (fun query -> query window wm) queries
let any queries = fun window wm -> List.exists (fun query -> query window wm) queries
let always : bool t = return true
let nop : unit t = return ()

let compose hook1 hook2 =
  fun window wm ->
  hook1 window wm;
  hook2 window wm
;;

let compose_all hooks = List.fold_left compose nop hooks
let when_ query hook = fun window wm -> if query window wm then hook window wm

let do_float : unit t =
  fun window _ -> window.presentation <- Floating Floating_rect.default
;;

let do_float_rect rect : unit t = fun window _ -> window.presentation <- Floating rect
let will_float = fun window _ -> Window.is_floating window
let do_tile = fun (window : Window.t) _ -> window.presentation <- Tiled
let do_shift tag = fun window wm -> Wm.windows (Window_set.shift_window tag window) wm
let do_f f = fun _ wm -> Wm.windows f wm

let first_match rules =
  fun window wm ->
  match List.find_opt (fun (query, _) -> query window wm) rules with
  | Some (_, hook) -> hook window wm
  | None -> ()
;;

module Infix = struct
  let ( ==> ) = when_
  let ( +> ) = compose
  let ( =? ) query str = is query str
  let ( =~ ) query substr = contains query substr
  let ( && ) query1 query2 = fun w wm -> query1 w wm && query2 w wm
  let ( || ) query1 query2 = fun w wm -> query1 w wm || query2 w wm
  let ( >>= ) = bind
  let ( >|= ) q f = map f q
end

module Syntax = struct
  let ( let+ ) q f = map f q
  let ( and+ ) = both
  let ( let* ) = bind
  let ( and* ) = both
end
