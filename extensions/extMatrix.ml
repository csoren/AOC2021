open Batteries

type 'a t = 'a array array

let make columns rows init =
  Array.make_matrix columns rows init

let width (m: 'a t) = Array.length m

let column n (m: 'a t) =
  Array.get m n |> Array.to_list

let columns (m: 'a t) =
  Array.to_list m |> List.map Array.to_list

let height (m: 'a t) =
  Array.get m 0 |> Array.length 

let row n (m: 'a t) =
  List.range 0 `To (width m - 1)
  |> List.fold_left (fun acc column -> m.(column).(n) :: acc) []
  |> List.rev

let transpose (m: 'a t) =
  List.range 0 `To (height m - 1)
  |> List.map (Fun.flip row m %> Array.of_list)
  |> Array.of_list

let rows (m: 'a t) =
  transpose m |> columns

let of_columns cols =
  List.map Array.of_list cols |> Array.of_list
    
let of_rows rows =
  of_columns rows |> transpose

let filter_rows fn (m: 'a t) =
  rows m |> List.filter fn |> of_rows