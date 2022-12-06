open Batteries
open Extensions


let input =
  File.lines_of "puzzle-input" |> List.of_enum

let char_is_one = (=) '1'

let bits =
  input |> List.map (String.to_list %> List.map char_is_one) |> Matrix.of_rows

let count_ones =
  List.count_matching Fun.id

let most_common_bit l =
  count_ones l >= List.length l / 2

let least_common_bit l =
  count_ones l < List.length l / 2

let bits_to_int =
  List.fold_left (fun acc bit -> (acc * 2) + Bool.to_int bit) 0


(* Part 1 *)

module Part1 = struct
  let most_common_bits =
    Matrix.columns bits |> List.map most_common_bit

  let inverted_bits =
    most_common_bits |> List.map Bool.not

  let (gamma, epsilon) =
    (most_common_bits |> bits_to_int, inverted_bits |> bits_to_int)

  let print_solution () =
    Printf.printf "Part 1, result %d\n" (gamma * epsilon)
end


(* Part 2 *)

module Part2 = struct
  let most_common_column_bit column m =
    Matrix.column column m |> most_common_bit
  
  let least_common_column_bit column m =
    Matrix.column column m |> least_common_bit

  let filter_bit common_bit m column =
    let bit = common_bit column m in
    Matrix.filter_rows (fun row -> List.at row column = bit) m

  let filter_values common_bit =
    List.range 0 `To (Matrix.width bits - 1)
    |> List.grade (filter_bit common_bit) bits 
    |> List.find_opt (Matrix.height %> (=) 1)
    |> Option.get

  let solve common_bit =
    filter_values common_bit |> Matrix.row 0 |> bits_to_int

  let oxygen = solve most_common_column_bit

  let co2 = solve least_common_column_bit

  let print_solution () =
    Printf.printf "Oxygen: %d\n" oxygen;
    Printf.printf "CO2: %d\n" co2;
    ()
end

let () =
  print_newline ();
  Part1.print_solution ();
  Part2.print_solution ()


