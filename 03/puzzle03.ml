open Batteries
open Extensions


let input =
  File.lines_of "puzzle-input" |> List.of_enum

let is_one = (=) '1'

let bits =
  input |> List.map (String.explode %> List.map is_one)

let transposed_bits =  List.transpose bits

let count_ones = List.count_matching Fun.id

let most_common_bit l =
  count_ones l > List.length l / 2

let most_common_bits list =
  let cons_bit el list = most_common_bit el :: list in
  List.fold_right cons_bit list []

let bits_to_int =
  List.fold_left (fun acc bit -> (acc * 2) + Bool.to_int bit) 0

let (gamma, epsilon) =
  let bits = most_common_bits transposed_bits in
  (bits |> bits_to_int, bits |> List.map Bool.not |> bits_to_int)

  
let print_part1 () =
  print_newline ();
  Printf.printf "Part 1, result %d\n" (gamma * epsilon)


let () =
  print_part1 ()
  