open Containers

type 'a t = Wm.t -> Window.t -> 'a

let return x = fun _ _ -> x
let map f q = fun wm window -> f (q wm window)
let ( let+ ) q f = map f q
let ( and+ ) a b = fun wm window -> a wm window, b wm window

let app_id : string t = fun _ window -> Option.get_or ~default:"" window.app_id
let title : string t = fun _ window -> Option.get_or ~default:"" window.title
let has_parent : bool t = fun _ window -> Option.is_some window.parent
let is_fixed_size : bool t = fun _ window -> Window.is_fixed_sized window

let is query s = map (String.equal s) query
let contains query sub = map (fun v -> String.mem ~sub v) query
let app_id_is = is app_id
let title_is = is title

let not query = map not query
let all queries = fun wm window -> List.for_all (fun query -> query wm window) queries
let any queries = fun wm window -> List.exists (fun query -> query wm window) queries

let always = fun _ _ -> true

let ( =? ) query str = is query str
let ( =~ ) query substr = contains query substr
let ( && ) query1 query2 = fun wm w -> query1 wm w && query2 wm w
let ( || ) query1 query2 = fun wm w -> query1 wm w || query2 wm w


