include ExtList.List

let unfold f seed =
  let rec unfold' f seed acc =
    match f seed with
    | None -> acc
    | Some (x, new_seed) -> (unfold' [@tailcall]) f new_seed (x :: acc)
  in
  rev @@ unfold' f seed []

let generate f =
  let rec generate' f acc =
    match f () with
    | None -> acc
    | Some x -> (generate' [@tailcall]) f (x :: acc)
  in
  rev @@ generate' f []

let count pred list =
  fold_left (fun acc el -> if pred el then acc + 1 else acc) 0 list

let to_string conv list =
  let elements = map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let int_list_to_string = to_string string_of_int
let int_list_list_to_string = to_string int_list_to_string

let of_textfile_lines file =
  let channel = open_in file in
  let result = generate (fun () -> Io.line_opt channel) in
  close_in channel;
  result

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
  
let sum op list =
  match list with
  | [] -> failwith "Empty list"
  | h :: tail -> fold_left op h tail

