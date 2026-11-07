include Types.Config
open Proto
open Containers

let workspaces = List.range 0 9 |> List.map Int.to_string

let keys =
  let open River_seat.Modifiers in
  let open Xkbcommon.Keysym in
  let open Int32 in
  let open Layouts in
  let open Wm in
  let digit_keys = [ K_0; K_1; K_2; K_3; K_4; K_5; K_6; K_7; K_8; K_9 ] in
  [ (mod4, K_Down), Wm.windows Window_set.focus_down
  ; (mod4, K_Up), windows Window_set.focus_up
  ; (mod4 lor shift, K_Down), windows Window_set.swap_down
  ; (mod4 lor shift, K_Up), windows Window_set.swap_up
  ; (mod4 lor shift, K_c), kill_focused_window
  ; (mod4, K_m), windows Window_set.focus_master
  ; (mod4 lor shift, K_m), windows Window_set.swap_master
  ; (mod4, K_Left), send_message Shrink
  ; (mod4, K_Right), send_message Expand
  ; (mod4, K_comma), send_message (IncMasterN 1)
  ; (mod4, K_period), send_message (IncMasterN (-1))
  ; (mod4, K_n), send_message NextLayout
  ; (mod4, K_t), tile_window
  ; (mod4, K_f), float_window
  ; (mod4, K_Return), spawn "alacritty"
  ; (mod4, K_space), spawn "noctalia msg panel-toggle launcher"
  ; (mod4, K_XF86AudioRaiseVolume), spawn "noctalia msg volume-up"
  ; (mod4, K_XF86AudioLowerVolume), spawn "noctalia msg volume-down"
  ; (mod4, K_XF86AudioMute), spawn "noctalia msg volume-mute"
  ; (mod4, K_XF86MonBrightnessUp), spawn "noctalia msg brightness-up"
  ; (mod4, K_XF86MonBrightnessDown), spawn "noctalia msg brightness-down"
  ; (mod4 lor shift, K_q), exit_wm
  ]
  @ List.map2
      (fun workspace key -> (mod4, key), windows (Window_set.view workspace))
      workspaces
      digit_keys
  @ List.map2
      (fun workspace key -> (mod4 lor shift, key), windows (Window_set.shift workspace))
      workspaces
      digit_keys
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

let layout =
  let open Layouts in
  let tiled_layout = tall 1 0.03 0.5 in
  tiled_layout ||| mirror tiled_layout ||| full
;;

let startup_hook = Wm.spawn "noctalia"

let manage_hook = 
  let open Manage_hook in
  let open Query in
  let do_shift_to_parent = fun wm window ->
    Wm.parent_workspace window wm
    |> Option.iter (fun tag -> Wm.windows (Window_set.shift_window tag window) wm)
  in
  let do_use_ssd = fun _ (window : Window.t) -> Proto.River_window.use_ssd window.proxy in
  compose_all
    [ always ==> do_use_ssd
    ; has_parent ==> do_shift_to_parent +> do_float 
    ; is_fixed_size ==> do_float
    ; app_id =? "org.squidowl.halloy" ==> do_float
    ]

let default =
  { keys
  ; pointer_bindings
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
