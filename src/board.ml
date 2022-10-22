type board = Piece.piece list

exception InvalidMove

let init_board board =
  let rec row x = function
    | [ [] ] -> []
    | h :: t ->
        let rec col y = function
          | [] -> []
          | h2 :: t2 ->
              if h2 = "-" then col (y + 1) t2
              else
                let piece_and_color = Piece.string_to_piece h2 in
                Piece.create_piece (fst piece_and_color)
                  (Some (char_of_int (x + 96), y))
                  (snd piece_and_color)
                :: col (y + 1) t2
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
        else
          try
            (List.find
               (fun a -> Piece.get_position a = Some (char_of_int (x + 96), y))
               lst
            |> Piece.piece_to_string)
            :: col (y + 1)
          with Not_found ->
            let square = if (x + y) mod 2 = 0 then "■" else "□" in
            square :: col (y + 1)
      in
      col 1 :: row (x + 1)
  in
  row 1

let remove_piece (board : Piece.piece list) (piece : Piece.piece) =
  List.filter (fun x -> x <> piece) board

let add_piece (board : Piece.piece list) (piece : Piece.piece) = piece :: board

let get_piece (board : board) (pos : (char * int) option) =
  List.find (fun x -> Piece.get_position x = pos) board

let clear_vertical board piece old_pos new_pos =
  let board' =
    List.filter
      (fun x -> Piece.is_in_vertical old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 1

let clear_horizontal board piece old_pos new_pos =
  let board' =
    List.filter
      (fun x -> Piece.is_in_horizontal old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 1

let clear_diagonal board piece old_pos new_pos =
  let board' =
    List.filter
      (fun x -> Piece.is_in_diagonal old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 1

let clear_path board old_pos piece new_pos =
  if Piece.is_rook piece then
    clear_horizontal board piece old_pos new_pos
    || clear_vertical board piece old_pos new_pos
  else if Piece.is_bishop piece then clear_diagonal board piece old_pos new_pos
  else if Piece.is_queen piece then
    clear_diagonal board piece old_pos new_pos
    || clear_horizontal board piece old_pos new_pos
    || clear_vertical board piece old_pos new_pos
  else true

(* Do you think we could improve the time efficiency of this?*)
let move (board : Piece.piece list) (old_pos : (char * int) option)
    (new_pos : (char * int) option) : board =
  let (piece : Piece.piece) = get_piece board old_pos in
  if
    Piece.valid_move piece new_pos
    && clear_path board (Piece.get_position piece) piece new_pos
  then
    let piece' = Piece.move_piece piece new_pos in
    let board' = remove_piece board piece in
    let board'' = add_piece board' piece' in
    let captured_piece =
      List.find (fun x -> Piece.get_position x = new_pos) (List.rev board'')
    in
    if captured_piece = piece' then board''
    else if Piece.get_color piece' = Piece.get_color captured_piece then
      raise InvalidMove
    else if
      (Piece.is_pawn piece' && Piece.valid_pawn_attack piece new_pos)
      || Piece.is_pawn piece' = false
    then
      let board''' = remove_piece board'' captured_piece in
      add_piece board''' (Piece.capture_piece captured_piece piece')
    else raise InvalidMove
  else raise InvalidMove

let graveyard_list = List.filter (fun x -> Piece.get_position x = None)