include ExtList.List

let count pred list =
  List.fold_left (fun acc el -> if pred el then acc + 1 else acc) 0 list

let to_string conv list =
  let elements = List.map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let of_textfile_lines file =
  let channel = open_in file in
  let rec loop () =
    try
      let next = input_line channel in
      next :: loop ()
    with End_of_file ->
      close_in channel;
      []
    in
  loop ()

let window size list =
  let rec slide tuple list =
    match list with
    | [] -> []
    | el :: tail ->
      let n = (drop 1 tuple) @ [el] in
      n :: slide n tail
    in

  let w = take size list in
  if length w < size then []
  else w :: slide w (drop size list)

let sum op list =
  match list with
  | [] -> failwith "Empty list"
  | h :: tail -> List.fold_left (fun acc el -> op acc el) h tail
