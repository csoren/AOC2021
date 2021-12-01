module List = ListExtensions


let puzzle_data =
  List.of_textfile_lines "puzzle-input"
  |> List.map int_of_string


let test_data = [
  199; 200; 208; 210; 200; 207; 240; 269; 260; 263;
]


let int_list_to_string = List.to_string string_of_int
let int_list_list_to_string = List.to_string int_list_to_string


(** [delta list] creates a list of the rate of change between successive elements of [list]  *)
let delta list =
  let sub = function
    | lhs :: rhs :: [] -> rhs - lhs
    | _ -> 0
  in
  List.window 2 list |> List.map sub


let positive_deltas list =
  list |> delta |> List.count (fun v -> v > 0)


let puzzle1 () =
  Printf.printf "Test data: %d\n" (test_data |> positive_deltas);
  Printf.printf "Puzzle data: %d\n" (puzzle_data |> positive_deltas)


let triplet_sums list =
  List.window 3 list |> List.map (List.sum (+))

  
let puzzle2 () =
  Printf.printf "Test data: %d\n" (test_data |> triplet_sums |> positive_deltas);
  Printf.printf "Puzzle data: %d\n" (puzzle_data |> triplet_sums |> positive_deltas)
  
  
let () =
  puzzle1 ();
  puzzle2 ()
