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

let add_piece (board : Piece.piece list) (piece : Piece.piece) =
  board @ [ piece ]

let get_piece (board : board) (pos : (char * int) option) =
  List.find (fun x -> Piece.get_position x = pos) board

let clear_vertical board old_pos new_pos =
  let board' =
    List.filter
      (fun x ->
        Validate.is_in_vertical_path old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 0

let clear_horizontal board old_pos new_pos =
  let board' =
    List.filter
      (fun x ->
        Validate.is_in_horizontal_path old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 0

let clear_diagonal board old_pos new_pos =
  let board' =
    List.filter
      (fun x ->
        Validate.is_in_diagonal_path old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 0

let clear_path board old_pos piece new_pos =
  if Piece.is_rook piece then
    clear_horizontal board old_pos new_pos
    || clear_vertical board old_pos new_pos
  else if Piece.is_bishop piece then clear_diagonal board old_pos new_pos
  else if Piece.is_queen piece then
    clear_diagonal board old_pos new_pos
    || clear_horizontal board old_pos new_pos
    || clear_vertical board old_pos new_pos
  else Piece.is_knight piece || Piece.is_king piece || Piece.is_pawn piece

let move (board : Piece.piece list) (old_pos : (char * int) option)
    (new_pos : (char * int) option) : board =
  let piece = get_piece board old_pos in
  let captured_piece_list =
    List.filter
      (fun x ->
        Piece.get_position x = new_pos
        &&
        if Piece.get_color x = Piece.get_color piece then raise InvalidMove
        else true)
      board
  in
  if
    Piece.valid_move piece new_pos
    && clear_path board old_pos piece new_pos
    && (Piece.is_pawn piece
        && List.length captured_piece_list = 1
        && Validate.valid_pawn_attack old_pos new_pos
       || (Piece.is_pawn piece && List.length captured_piece_list = 0)
       || Piece.is_pawn piece = false)
  then
    let piece' = Piece.move_piece piece new_pos in
    if List.length captured_piece_list = 1 then
      let captured_piece = List.nth captured_piece_list 0 in
      let captured_piece_updated = Piece.capture_piece captured_piece piece' in
      let board' = remove_piece board captured_piece in
      let board'' = add_piece board' captured_piece_updated in
      let board''' = remove_piece board'' piece in
      add_piece board''' piece'
    else add_piece (remove_piece board piece) piece'
  else raise InvalidMove

let graveyard_list = List.filter (fun x -> Piece.get_position x = None)