type t = unit Query.t

let nop = fun _ _ -> ()
let compose hook1 hook2 = fun wm window -> hook1 wm window; hook2 wm window
let compose_all hooks = fun wm window -> List.iter (fun hook -> hook wm window) hooks
let when_ query hook = fun wm window -> if query wm window then hook wm window

let do_float = fun _ (window : Window.t) -> window.presentation <- Floating Floating_rect.default
let do_float_rect rect = fun _ (window : Window.t) -> window.presentation <- Floating rect
let do_tile = fun _ (window : Window.t) -> window.presentation <- Tiled
let do_shift tag = fun wm (window : Window.t) -> Wm.windows (Window_set.shift_window tag window) wm
let do_f f = f

let ( ==> ) = when_
let ( +> ) = compose 

let first_match rules =
  fun wm window ->
    match List.find_opt (fun (query, _) -> query wm window) rules with
    | Some (_, hook) -> hook wm window
    | None -> ()
