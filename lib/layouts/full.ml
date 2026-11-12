type t = unit

let description _ = "Full"
let handle_message _ _ _ = None

let run_layout _ (workspace : _ Window_set.workspace) rect =
  match workspace.stack with
  | None -> [], None
  | Some stack -> [ rect, stack.focus ], None
;;
