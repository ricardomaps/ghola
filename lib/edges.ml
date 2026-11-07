include Proto.River_edges
open Containers
open Int32

let all = bottom lor top lor left lor right
let contains edge edges = edges land edge <> 0l
let union edge1 edge2 = edge1 lor edge2
