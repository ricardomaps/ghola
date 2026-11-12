type t =
  { nmaster : int
  ; ratio_increment : float
  ; ratio : float
  }

type Message.t += IncMasterN of int | Expand | Shrink

let description _ = "Tall"

let tile n nmaster ratio rect =
  let open Rect in
  if nmaster >= n || nmaster = 0
  then split_vertically n rect
  else (
    (* named pupil so we stay woke *)
    let master_rect, pupil_rect = split_horizontally_by ratio rect in
    let masters = split_vertically nmaster master_rect in
    let pupils = split_vertically (n - nmaster) pupil_rect in
    masters @ pupils)
;;

let run_layout _ (workspace : _ Window_set.workspace) rect =
  match workspace.stack with
  | None -> [], None
  | Some stack ->
    let { ratio; nmaster; _ } = workspace.layout in
    let windows = Zipper.to_list stack in
    let rects = tile (List.length windows) nmaster ratio rect in
    List.combine rects windows, None
;;

let handle_message _ l = function
  | IncMasterN n -> Some { l with nmaster = l.nmaster + n }
  | Expand -> Some { l with ratio = Float.min 1. (l.ratio +. l.ratio_increment) }
  | Shrink -> Some { l with ratio = Float.max 0. (l.ratio -. l.ratio_increment) }
  | _ -> None
;;
