open Proto
open Containers

type t =
  { proxy : [ `V4 ] River_seat.t
  ; layer_shell_proxy : [ `V1 ] River_layer_shell_seat.t
  ; mutable bindings : [ `V2 ] River_xkb_binding.t list
  ; mutable pointer_bindings : [ `V4 ] River_pointer_binding.t list
  ; mutable operation : Operation.t option
  }

let equal : t -> t -> bool = Equal.physical

let create
      proxy
      ~keys
      ~pointer_bindings
      ~xkb_handler
      ~pointer_handler
      ~seat_handler
      ~shell_handler
      ~bindings_proxy
      ~shell_proxy
  =
  let bindings =
    List.map
      (fun ((modifiers, keysym), action) ->
         let keysym = Int32.of_int @@ Xkbcommon.Keysym.to_int keysym in
         River_xkb_bindings.get_xkb_binding
           bindings_proxy
           (xkb_handler action)
           ~seat:proxy
           ~modifiers
           ~keysym)
      keys
  in
  let pointer_bindings =
    List.map
      (fun ((modifiers, button), action) ->
         let button = Button.to_int32 button in
         River_seat.get_pointer_binding proxy (pointer_handler action) ~modifiers ~button)
      pointer_bindings
  in
  let layer_shell_proxy =
    River_layer_shell.get_seat shell_proxy ~seat:proxy shell_handler
  in
  let seat = { proxy; bindings; layer_shell_proxy; operation = None; pointer_bindings } in
  Wayland.Proxy.Handler.attach proxy (seat_handler seat);
  seat
;;

let destroy seat =
  List.iter
    (fun binding ->
       River_xkb_binding.destroy binding;
       Wayland.Proxy.delete binding)
    seat.bindings;
  List.iter
    (fun pointer_binding ->
       River_pointer_binding.destroy pointer_binding;
       Wayland.Proxy.delete pointer_binding)
    seat.pointer_bindings;
  River_seat.destroy seat.proxy;
  Wayland.Proxy.delete seat.proxy
;;
