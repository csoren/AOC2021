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
  let rec loop () =
    try
      let next = input_line channel in
      next :: loop ()
    with End_of_file ->
      close_in channel;
      []
    in
  loop ()

let rec window size list =
  let w = take size list in
  if length w < size then []
  else w :: window size (drop 1 list)

let sum op list =
  match list with
  | [] -> failwith "Empty list"
  | h :: tail -> fold_left op h tail
