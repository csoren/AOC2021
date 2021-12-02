include ExtList.List

let count pred list =
  fold_left (fun acc el -> if pred el then acc + 1 else acc) 0 list

let to_string conv list =
  let elements = map conv list |> String.concat "; " in
  "[" ^ elements ^ "]"
  
let int_list_to_string = to_string string_of_int
let int_list_list_to_string = to_string int_list_to_string

let of_textfile_lines file =
  let channel = open_in file in
  let rec loop acc =
    match
      try Some (input_line channel)
      with End_of_file -> None
    with
    | None -> acc
    | Some v -> (loop [@tailcall]) (v :: acc)
  in
  rev @@ loop []

let window size list =
  let rec window' list acc = 
    let w = take size list in
    if length w < size then acc
    else (window' [@tailcall]) (drop 1 list) (w :: acc)
  in
  rev (window' list [])

let sum op list =
  match list with
  | [] -> failwith "Empty list"
  | h :: tail -> fold_left op h tail
