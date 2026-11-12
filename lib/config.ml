include Types.Config
open Proto
open Containers

let keys { modifier; terminal; workspaces; _ } =
  let open River_seat.Modifiers in
  let open Xkbcommon.Keysym in
  let open Int32 in
  let open Layouts in
  let open Wm in
  let digit_keys = [ K_0; K_1; K_2; K_3; K_4; K_5; K_6; K_7; K_8; K_9 ] in
  [ (modifier, K_Down), windows Window_set.focus_down
  ; (modifier, K_Up), windows Window_set.focus_up
  ; (modifier lor shift, K_Down), windows Window_set.swap_down
  ; (modifier lor shift, K_Up), windows Window_set.swap_up
  ; (modifier lor shift, K_c), kill_focused_window
  ; (modifier, K_m), windows Window_set.focus_master
  ; (modifier lor shift, K_m), windows Window_set.swap_master
  ; (modifier, K_Left), send_message Shrink
  ; (modifier, K_Right), send_message Expand
  ; (modifier, K_comma), send_message (IncMasterN 1)
  ; (modifier, K_period), send_message (IncMasterN (-1))
  ; (modifier, K_n), send_message NextLayout
  ; (modifier, K_t), tile_window
  ; (modifier, K_f), float_window
  ; (modifier, K_Return), spawn terminal
  ; (modifier lor shift, K_q), exit_wm
  ]
  @ (List.combine_shortest workspaces digit_keys
     |> List.map (fun (workspace, key) ->
       (mod4, key), windows (Window_set.view workspace)))
  @ (List.combine_shortest workspaces digit_keys
     |> List.map (fun (workspace, key) ->
       (mod4 lor shift, key), windows (Window_set.shift workspace)))
;;

let pointer_bindings =
  let open River_seat.Modifiers in
  let open Window_set in
  let open Wm in
  let button1, button2 = 0x110l, 0x111l in
  [ ( (mod4, button1)
    , fun window wm ->
        windows (focus_window window) wm;
        move_window window wm;
        windows shift_master wm )
  ; ( (mod4, button2)
    , fun window wm ->
        windows (focus_window window) wm;
        resize_window window wm;
        windows shift_master wm )
  ]
;;

let workspaces = List.range 0 9 |> List.map Int.to_string

let layout =
  let open Layouts in
  let tiled_layout = tall ~nmaster:1 ~ratio:0.5 ~ratio_increment:0.03 in
  tiled_layout ||| mirror tiled_layout ||| full
;;

let startup_hook _ = ()

let manage_hook =
  let open Manage_hook in
  let do_use_ssd : unit t = fun window _ -> Proto.River_window.use_ssd window.proxy in
  compose_all Infix.[ do_use_ssd; app_id =? "org.squidowl.halloy" ==> do_float ]
;;

let default =
  { keys
  ; pointer_bindings
  ; terminal = "alacritty"
  ; modifier = River_modifier.mod4
  ; layout
  ; workspaces
  ; startup_hook
  ; manage_hook
  ; focus_follows_mouse = false
  ; border_width = 2
  ; normal_border_color = Color.of_int 0x000000FF
  ; focused_border_color = Color.of_int 0xFF0000FF
  }
;;
