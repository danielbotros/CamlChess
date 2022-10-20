type board = Piece.piece list

let init_board board =
  let rec row x = function
    | [ [] ] -> []
    | h :: t ->
        let rec col y = function
          | [] -> []
          | h2 :: t2 ->
              if h2 = "-" then col (y + 1) t2
              else Piece.create_piece h2 x y :: col (y + 1) t2
        in
        col 1 h @ row (x + 1) t
    | _ -> []
  in
  row 1 board

let board_to_list lst =
  let rec row x =
    if x = 9 then []
    else
      let rec col y =
        if y = 9 then []
        else if
          List.exists
            (fun a -> Piece.get_position a = Some (char_of_int (x + 96), y))
            lst
        then "P" :: col (y + 1) (** this only works for pawns*)
        else "-" :: col (y + 1)
      in
      col 1 :: row (x + 1)
  in
  row 1
