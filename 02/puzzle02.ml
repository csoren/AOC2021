open Extensions
open Parser

let test_commands = commands_of_list [
  "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"; 
]

let puzzle_commands = Extlist.of_textfile_lines "puzzle-input" |> commands_of_list


module Star1 = struct
  let new_position (horizontal, depth) = function
  | Forward x -> (horizontal + x, depth)
  | Down x -> (horizontal, depth + x)
  | Up x -> (horizontal, depth - x)

  let process_commands = List.fold_left new_position (0,0)
end


module Star2 = struct
  let new_position (horizontal, depth, aim) = function
  | Forward x -> (horizontal + x, depth + aim * x, aim)
  | Down x -> (horizontal, depth, aim + x)
  | Up x -> (horizontal, depth, aim - x)

  let process_commands commands =
    let (horizontal, depth, _) = List.fold_left new_position (0,0,0) commands in
    (horizontal, depth)
end


let print_result name (horizontal, depth) =
  Printf.printf "%s: horizontal %d, depth %d, result = %d\n" name horizontal depth (horizontal * depth)

  
let () =
  print_result "(1) Test data" (Star1.process_commands test_commands);
  print_result "(1) Puzzle data" (Star1.process_commands puzzle_commands);
  print_result "(2) Test data" (Star2.process_commands test_commands);
  print_result "(2) Puzzle data" (Star2.process_commands puzzle_commands)
