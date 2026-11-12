module Make (L : Layout.S) = struct
  type t = L.t

  let description l = "Mirror " ^ L.description l
  let handle_message = L.handle_message

  let run_layout wm workspace rect =
    let rs, u = L.run_layout wm workspace (Rect.mirror rect) in
    let rs = List.map (fun (r, w) -> Rect.mirror r, w) rs in
    rs, u
  ;;
end
