open Proto
open Containers

type t =
  { proxy : [ `V4 ] River_output.t
  ; layer_shell_proxy : [ `V1 ] River_layer_shell_output.t
  ; mutable geometry : Rect.t
  ; mutable working_geometry : Rect.t
  }

let create proxy ~output_handler ~shell_proxy ~shell_handler =
  let layer_shell_proxy =
    River_layer_shell.get_output shell_proxy ~output:proxy (shell_handler proxy)
  in
  let output =
    { proxy; geometry = Rect.empty; layer_shell_proxy; working_geometry = Rect.empty }
  in
  Wayland.Proxy.Handler.attach proxy (output_handler output);
  output
;;

let destroy output =
  River_output.destroy output.proxy;
  Wayland.Proxy.delete output.proxy;
  River_layer_shell_output.destroy output.layer_shell_proxy;
  Wayland.Proxy.delete output.layer_shell_proxy
;;

let equal : t -> t -> bool = Equal.physical
