open Batteries
open Extensions


let input =
  File.lines_of "puzzle-input" |> List.of_enum

let string_list_to_card =
  List.map (String.split_on_char_greedy ' ' %> List.map int_of_string) %> Matrix.of_rows

let (bag, cards) =
  let sections = List.group_at ~separator:String.is_empty input in
  ( List.hd sections |> List.hd |> String.split_on_char ',' |> List.map int_of_string,
    List.map string_list_to_card (List.tl sections)
  )

let check_list_bingo drawn =
  List.for_all (Fun.flip Set.contains drawn)

let find_list_bingo drawn =
  List.find_opt (check_list_bingo drawn)

let check_card_bingo drawn card =
  Matrix.columns card |> find_list_bingo drawn
  |> Option.or_else (fun () -> Matrix.rows card |> find_list_bingo drawn)

let check_cards_bingo drawn =
  List.find_opt (check_card_bingo drawn %> Option.is_some)

module Part1 = struct
  let bingo cards =
    let rec bingo' drawn bag cards =
      let called_number = List.hd bag in
      let bag' = List.tl bag in
      let drawn' = Set.add called_number drawn in
      match check_cards_bingo drawn' cards with
      | None -> bingo' drawn' bag' cards
      | Some card -> Some (card, called_number, drawn')
    in
    bingo' Set.empty bag cards |> Option.get
  
  let sum_not_drawn card drawn =
    let sum_row =
      List.filter (fun v -> not @@ Set.contains v drawn) %> List.sum in
    Matrix.map_rows sum_row card |> List.sum

  let score cards =
    let (card, called_number, drawn) = bingo cards in
    let card_sum = sum_not_drawn card drawn in
    called_number * card_sum

  let print () =
    Printf.printf "Part 1, final score: %d\n" (score cards)

end

let () =
  print_newline ();
  Part1.print ()
