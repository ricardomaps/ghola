(* shamelessly copied from oxbow *)
type t =
  | Key_unknown 
  | Btn_0 
  | Btn_1 
  | Btn_2 
  | Btn_3 
  | Btn_4 
  | Btn_5 
  | Btn_6 
  | Btn_7 
  | Btn_8 
  | Btn_9 
  | Btn_left 
  | Btn_right 
  | Btn_middle 
  | Btn_side 
  | Btn_extra 
  | Btn_forward 
  | Btn_back 
  | Btn_task 

let to_int32 = function
  | Btn_0 -> 0x100l
  | Btn_1 -> 0x101l
  | Btn_2 -> 0x102l
  | Btn_3 -> 0x103l
  | Btn_4 -> 0x104l
  | Btn_5 -> 0x105l
  | Btn_6 -> 0x106l
  | Btn_7 -> 0x107l
  | Btn_8 -> 0x108l
  | Btn_9 -> 0x109l
  | Btn_left -> 0x110l
  | Btn_right -> 0x111l
  | Btn_middle -> 0x112l
  | Btn_side -> 0x113l
  | Btn_extra -> 0x114l
  | Btn_forward -> 0x115l
  | Btn_back -> 0x116l
  | Btn_task -> 0x117l
  | Key_unknown -> 240l
;;
