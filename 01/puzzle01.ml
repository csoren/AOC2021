open Extensions

let puzzle_data =
  List.of_textfile_lines "puzzle-input"
  |> List.map int_of_string


let test_data = [
  199; 200; 208; 210; 200; 207; 240; 269; 260; 263;
]


(** [delta list] creates a list of the change between successive elements of [list]  *)
let delta list =
  List.window 2 list |> List.map (List.sum @@ Fun.flip (-))

(** [positive_deltas list] counts the positive changes between successive elements of [list] *)
let positive_deltas list =
  list |> delta |> List.count (fun v -> v > 0)

(** [triplet_sums list] sums the triplets found in a three element sliding window in [list] *)  
let triplet_sums list =
  List.window 3 list |> List.map (List.sum (+))
  
    
let puzzle1 () =
  Printf.printf "(1) Test data: %d\n" (test_data |> positive_deltas);
  Printf.printf "(1) Puzzle data: %d\n" (puzzle_data |> positive_deltas)


let puzzle2 () =
  Printf.printf "(2) Test data: %d\n" (test_data |> triplet_sums |> positive_deltas);
  Printf.printf "(2) Puzzle data: %d\n" (puzzle_data |> triplet_sums |> positive_deltas)
  
  
let () =
  puzzle1 ();
  puzzle2 ()
