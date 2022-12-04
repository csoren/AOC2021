open Batteries
open Extensions


let input = File.lines_of "puzzle-input" |> List.of_enum

let char_to_bit ch = int_of_char ch - (int_of_char '0')

let heads = List.map List.hd

let tails = List.map List.tl

let rec transpose = function
  | [] :: _ -> []
  | l -> heads l :: (tails l |> transpose)

let bits =
  input |> List.map (String.explode %> List.map char_to_bit)

let transposed_bits = transpose bits

let count_ones = List.count_matching ((=) 1)
