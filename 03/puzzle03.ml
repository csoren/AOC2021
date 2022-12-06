open Batteries
open Extensions


let input =
  File.lines_of "puzzle-input" |> List.of_enum

  (*
let input = [ 
  "00100"; "11110"; "10110"; "10111"; "10101"; "01111"; "00111"; "11100"; "10000"; "11001"; "00010"; "01010" ]
*)

let char_is_one = (=) '1'

let bits m =
  m |> List.map (String.to_list %> List.map char_is_one) |> Matrix.of_rows

let count_ones = List.count_matching Fun.id

let most_common_bit l =
  let count = count_ones l in
  count >= (List.length l - count)

let least_common_bit l =
  let count = count_ones l in
  count < (List.length l - count)

let bits_to_int =
  List.fold_left (fun acc bit -> (acc * 2) + Bool.to_int bit) 0


(* Part 1 *)

module Part1 = struct
  let most_common_bits m =
    Matrix.columns m |> List.map most_common_bit

  let inverted_bits m =
    most_common_bits m |> List.map Bool.not

  let print_solution () =
    let m = bits input in
    let gamma = most_common_bits m |> bits_to_int in
    let epsilon = inverted_bits m |> bits_to_int in
    Printf.printf "Part 1, result %d\n" (gamma * epsilon)
end


(* Part 2 *)

module Part2 = struct
  let most_common_column_bit column m =
    Matrix.column column m |> most_common_bit
  
  let least_common_column_bit column m =
    Matrix.column column m |> least_common_bit

  let filter_rows column_index m bit =
    let match_row row = List.at row column_index = bit in
    Matrix.filter_rows match_row m

  let filter_by_column most_common_bit_of m column_index =
    match Matrix.height m with
    | 0 | 1 -> m
    | _ -> most_common_bit_of column_index m |> filter_rows column_index m

  let filter_values most_common_bit_of m =
    List.range 0 `To (Matrix.width m - 1)
    |> List.grade (filter_by_column most_common_bit_of) m
    |> List.last

  let solve most_common_bit_of =
    bits input |> filter_values most_common_bit_of |> Matrix.row 0 |> bits_to_int

  let (oxygen, co2) =
    (solve most_common_column_bit, solve least_common_column_bit)

  let print_solution () =
    Printf.printf "Part 2, result %d\n" (oxygen * co2)
end

let () =
  print_newline ();
  Part1.print_solution ();
  Part2.print_solution ()
