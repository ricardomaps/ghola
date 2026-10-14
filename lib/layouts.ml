open Wm

module Full = struct
  type t = unit

  let description _ = "Full"
  let handle_message _ _ _ = None

  let run_layout _ (workspace : _ Stack_set.workspace) rect =
    match workspace.stack with
    | None -> [], None
    | Some stack -> [ rect, stack.focus ], None
  ;;
end

module Mirror (L : Layout.S) = struct
  type t = L.t

  let description l = "Mirror " ^ L.description l
  let handle_message = L.handle_message

  let run_layout wm workspace rect =
    let rs, u = L.run_layout wm workspace rect in
    let rs = List.map (fun (r, w) -> Rect.mirror r, w) rs in
    rs, u
  ;;
end

type message += IncMasterN of int | Expand | Shrink

module Tall = struct
  type t =
    { nmaster : int
    ; ratio_increment : float
    ; ratio : float
    }

  let description _ = "Tall"

  let tile n nmaster ratio rect =
    let open Rect in
    if nmaster <= n || nmaster == 0
    then split_vertically n rect
    else (
      (* named pupil so we stay woke *)
      let master_rect, pupil_rect = split_horizontally_by ratio rect in
      let masters = split_vertically nmaster master_rect in
      let pupils = split_vertically (n - nmaster) pupil_rect in
      masters @ pupils)
  ;;

  let run_layout _ (workspace : _ Stack_set.workspace) rect =
    match workspace.stack with
    | None -> [], None
    | Some stack ->
      let { ratio; nmaster; _ } = workspace.layout in
      let windows = Stack.to_list stack in
      let rects = tile (List.length windows) nmaster ratio rect in
      List.combine rects windows, None
  ;;

  let handle_message _ l = function
    | IncMasterN n -> Some { l with nmaster = l.nmaster + n }
    | Expand -> Some { l with ratio = Float.min 1. (l.ratio +. l.ratio_increment) }
    | Shrink -> Some { l with ratio = Float.max 0. (l.ratio -. l.ratio_increment) }
    | _ -> None
  ;;
end

type message += FirstLayout | NextLayout | NextNoWrap | JumpToLayout of string

module Choose (L : Layout.S) (R : Layout.S) = struct
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

  let run_layout wm (workspace : _ Stack_set.workspace) rect =
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

let full : layout = Layout ((module Full), ())

let tall nmaster ratio_increment ratio =
  let l : Tall.t = { nmaster; ratio; ratio_increment } in
  Layout ((module Tall), l)
;;

let mirror (Layout ((module L), l)) = Layout ((module Mirror (L)), l)

let ( ||| ) (Layout ((module L), l)) (Layout ((module R), r)) =
  let module F = Choose (L) (R) in
  let t : F.t = { current = CL; left = l; right = r } in
  Layout ((module F), t)
;;
