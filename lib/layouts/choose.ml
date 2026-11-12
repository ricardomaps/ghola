open Message

type Message.t += FirstLayout | NextLayout | NextNoWrap | JumpToLayout of string

module Make (L : Layout.S) (R : Layout.S) = struct
  type clr =
    | CL
    | CR

  type t =
    { current : clr
    ; left : L.t
    ; right : R.t
    }

  let description t =
    match t.current with
    | CL -> L.description t.left
    | CR -> R.description t.right
  ;;

  let run_layout wm (workspace : _ Window_set.workspace) rect =
    let ({ current; left; right } as layout) = workspace.layout in
    match current with
    | CL ->
      let rects, left_opt = L.run_layout wm { workspace with layout = left } rect in
      let updated = Option.map (fun left -> { layout with left }) left_opt in
      rects, updated
    | CR ->
      let rects, right_opt = R.run_layout wm { workspace with layout = right } rect in
      let updated = Option.map (fun right -> { layout with right }) right_opt in
      rects, updated
  ;;

  let rec handle_message wm ({ current; left; right } as layout) = function
    | NextLayout ->
      (match handle_message wm layout NextNoWrap with
       | None -> handle_message wm layout FirstLayout
       | layout -> layout)
    | NextNoWrap ->
      (match current with
       | CR ->
         Option.map
           (fun right -> { layout with right })
           (R.handle_message wm right NextNoWrap)
       | CL ->
         (match L.handle_message wm left NextNoWrap with
          | Some left -> Some { layout with left }
          | None ->
            let left = L.handle_message wm left Hide |> Option.value ~default:left in
            let right =
              R.handle_message wm right FirstLayout |> Option.value ~default:right
            in
            Some { current = CR; left; right }))
    | FirstLayout ->
      let left_opt = L.handle_message wm left FirstLayout in
      (match current with
       | CL -> Option.map (fun left -> { layout with left }) left_opt
       | CR ->
         let right = R.handle_message wm right Hide |> Option.value ~default:right in
         let left = Option.value ~default:left left_opt in
         Some { current = CL; left; right })
    | JumpToLayout desc as msg ->
      let left_opt = L.handle_message wm left msg in
      let right_opt = R.handle_message wm right msg in
      let left = Option.value ~default:left left_opt in
      let right = Option.value ~default:right right_opt in
      let next =
        if L.description left = desc
        then CL
        else if R.description right = desc
        then CR
        else current
      in
      (match left_opt, right_opt, current, next with
       | None, None, x, y when x = y -> None
       | _, _, CL, CR ->
         let left = Option.value ~default:left (L.handle_message wm left Hide) in
         Some { current = next; left; right }
       | _, _, CR, CL ->
         let right = Option.value ~default:right (R.handle_message wm right Hide) in
         Some { current = next; left; right }
       | _ -> Some { current; left; right })
    | ReleaseResources ->
      let left_opt = L.handle_message wm left ReleaseResources in
      let right_opt = R.handle_message wm right ReleaseResources in
      (match left_opt, right_opt with
       | None, None -> None
       | _ ->
         let left = Option.value ~default:left left_opt in
         let right = Option.value ~default:right right_opt in
         Some { layout with left; right })
    | message ->
      (match current with
       | CL ->
         L.handle_message wm left message |> Option.map (fun left -> { layout with left })
       | CR ->
         R.handle_message wm right message
         |> Option.map (fun right -> { layout with right }))
  ;;
end
