open Batteries

type command =
  | Forward of int
  | Down of int
  | Up of int

let make_command direction distance =
  match direction with
  | "forward" -> Forward distance
  | "down" -> Down distance
  | "up" -> Up distance
  | _ -> failwith "Unknown direction"

let command_of_string s =
  let (direction, distance) = String.split s ~by:" " in
  make_command direction (int_of_string distance)
