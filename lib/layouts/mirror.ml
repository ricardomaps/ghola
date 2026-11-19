module Make (L : Glayout.S) = struct
  type wm = Wm.t
  type t = Mirror of L.t [@@deriving yojson]

  let description (Mirror l) = "Mirror " ^ L.description l

  let handle_message wm (Mirror l) msg =
    L.handle_message wm l msg |> Option.map (fun s -> Mirror s)
  ;;

  let run_layout wm Window_set.({ layout = Mirror l; _ } as workspace) rect =
    let workspace' = { workspace with layout = l } in
    let tiles, state = L.run_layout wm workspace' (Rect.mirror rect) in
    let tiles = List.map (Pair.map_fst Rect.mirror) tiles in
    let state = Option.map (fun s -> Mirror s) state in
    tiles, state
  ;;
end
