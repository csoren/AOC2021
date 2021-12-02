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
  match (String.split_on_char ' ' s) with
  | [ direction; distance ] -> make_command direction (int_of_string distance)
  | _ -> failwith "Can't parse command"

let commands_of_list list =
  list |> List.map command_of_string
