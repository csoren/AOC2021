open Extensions
open Parser

let test_commands = commands_of_list [
  "forward 5"; "down 5"; "forward 8"; "up 3"; "down 8"; "forward 2"; 
]

let puzzle_commands = List.of_textfile_lines "puzzle-input" |> commands_of_list


module Star1 = struct
  let new_position (horizontal, depth) = function
  | Forward x -> (horizontal + x, depth)
  | Down x -> (horizontal, depth + x)
  | Up x -> (horizontal, depth - x)

  let process_commands = List.fold_left new_position (0,0)

  let print_result name commands =
    let (horizontal, depth) = process_commands commands in
    Printf.printf "%s: horizontal %d, depth %d, result = %d\n" name horizontal depth (horizontal * depth)

  let solutions () =
    print_result "Test data" test_commands;
    print_result "Puzzle data" puzzle_commands
end


module Star2 = struct
  let new_position (horizontal, depth, aim) = function
  | Forward x -> (horizontal + x, depth + aim * x, aim)
  | Down x -> (horizontal, depth, aim + x)
  | Up x -> (horizontal, depth, aim - x)

  let process_commands = List.fold_left new_position (0,0,0)

  let print_result name commands =
    let (horizontal, depth, _) = process_commands commands in
    Printf.printf "%s: horizontal %d, depth %d, result = %d\n" name horizontal depth (horizontal * depth)

  let solutions () =
    print_result "Test data" test_commands;
    print_result "Puzzle data" puzzle_commands
end


let () =
  Star1.solutions ();
  Star2.solutions ()
