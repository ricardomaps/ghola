type t =
  { r : int32
  ; g : int32
  ; b : int32
  ; a : int32
  }

let of_int n =
  let a = n land 0xff in
  let r = (n lsr 24) land 0xff in
  let g = (n lsr 16) land 0xff in
  let b = (n lsr 8) land 0xff in
  let widen v = Int32.mul (Int32.of_int v) 0x01010101l in
  let premul c = ((c * a) + 127) / 255 in
  { r = widen (premul r); g = widen (premul g); b = widen (premul b); a = widen a }
;;
