open Batteries
include List

let to_string conv list =
  let elements = map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let int_list_to_string = to_string string_of_int

let int_list_list_to_string = to_string int_list_to_string

let window size list =
  let rec window' list acc = 
    let w = take size list in
    if length w < size then acc
    else (window' [@tailcall]) (drop 1 list) (w :: acc)
  in
  rev @@ window' list []

let grade f init list =
  let step (acc, x) el =
    let y = f x el in
    (y :: acc, y)
  in
  fold_left step ([], init) list |> fst |> rev

let window_triplets list =
  grade (fun (_,b,c) d -> (b,c,d)) (0,0,0) list
  |> drop 2

let window_tuplets list =
  grade (fun (_,b) c -> (b,c)) (0,0) list
  |> drop 1
