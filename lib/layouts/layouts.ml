open Types.Layout
open Message

let full = Layout ((module Full), ())

type Message.t +=
  | IncMasterN = Tall.IncMasterN
  | Expand = Tall.Expand
  | Shrink = Tall.Shrink

let tall ~nmaster ~ratio_increment ~ratio =
  let l : Tall.t = { nmaster; ratio; ratio_increment } in
  Layout ((module Tall), l)
;;

let mirror (Layout ((module L), l)) = Layout ((module Mirror.Make (L)), l)

type Message.t +=
  | FirstLayout = Choose.FirstLayout
  | NextLayout = Choose.NextLayout
  | NextNoWrap = Choose.NextNoWrap
  | JumpToLayout = Choose.JumpToLayout

let ( ||| ) (Layout ((module L), l)) (Layout ((module R), r)) =
  let module F = Choose.Make (L) (R) in
  let t : F.t = { current = CL; left = l; right = r } in
  Layout ((module F), t)
;;
