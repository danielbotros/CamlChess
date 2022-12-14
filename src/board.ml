type board = Piece.piece list

exception InvalidMove of string

let (empty : Piece.piece list) = []

let get_piece (board : board) (pos : (char * int) option) =
  try List.find (fun x -> Piece.get_position x = pos) board
  with Not_found ->
    raise
      (InvalidMove "[get_piece] error: No piece has this [pos] on the [board]")

let get_pieces (board : Piece.piece list) = board

let remove_piece (board : Piece.piece list) (piece : Piece.piece) =
  List.filter (fun x -> x <> piece) board

let add_piece (board : Piece.piece list) (piece : Piece.piece) = piece :: board

let graveyard (board : Piece.piece list) =
  List.map
    (fun x -> Piece.piece_to_string x)
    (List.filter (fun x -> Piece.get_position x = None) board)

let get_captured_piece board piece new_pos =
  List.filter
    (fun p ->
      Piece.get_position p = new_pos
      &&
      if Piece.get_color p = Piece.get_color piece then
        raise
          (InvalidMove
             "[get_captured_piece] error: attempting to capture own team's \
              piece at [new_pos].")
      else true)
    board

let init_board (gui_board : string list list) =
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
  row 1 gui_board

let board_to_list board =
  let rec row x =
    if x = 9 then []
    else
      let rec col y =
        if y = 9 then []
        else
          try
            (List.find
               (fun a -> Piece.get_position a = Some (char_of_int (x + 96), y))
               board
            |> Piece.piece_to_string)
            :: col (y + 1)
          with Not_found ->
            let square = if (x + y) mod 2 = 0 then "■" else "□" in
            square :: col (y + 1)
      in
      col 1 :: row (x + 1)
  in
  row 1

let board_info_to_list (board : Piece.piece list)
    (moves_lst : (char * int) option list) =
  let rec row x =
    if x = 9 then []
    else
      let rec col y =
        if y = 9 then []
        else
          try
            if
              List.exists
                (fun pos' -> pos' = Some (char_of_int (x + 96), y))
                moves_lst
            then "✓" :: col (y + 1)
            else
              (List.find
                 (fun a ->
                   Piece.get_position a = Some (char_of_int (x + 96), y))
                 board
              |> Piece.piece_to_string)
              :: col (y + 1)
          with Not_found ->
            let square =
              let pos = Some (char_of_int (x + 96), y) in
              if List.exists (fun pos' -> pos' = pos) moves_lst then "✓"
              else if (x + y) mod 2 = 0 then "■"
              else "□"
            in
            square :: col (y + 1)
      in
      col 1 :: row (x + 1)
  in
  row 1

let clear_vertical board old_pos new_pos =
  let board' =
    List.filter
      (fun x ->
        Validate.is_in_vertical_path old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 0 && Validate.vertical_move old_pos new_pos

let clear_horizontal board old_pos new_pos =
  let board' =
    List.filter
      (fun x ->
        Validate.is_in_horizontal_path old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 0 && Validate.horizontal_move old_pos new_pos

let clear_diagonal board old_pos new_pos =
  let board' =
    List.filter
      (fun x ->
        Validate.is_in_diagonal_path old_pos (Piece.get_position x) new_pos)
      board
  in
  List.length board' = 0 && Validate.diagonal_move old_pos new_pos

let clear_path board old_pos piece new_pos =
  if Piece.is_rook piece then
    clear_vertical board old_pos new_pos
    || clear_horizontal board old_pos new_pos
  else if Piece.is_bishop piece then clear_diagonal board old_pos new_pos
  else if Piece.is_queen piece then
    clear_diagonal board old_pos new_pos
    || clear_horizontal board old_pos new_pos
    || clear_vertical board old_pos new_pos
  else Piece.is_knight piece || Piece.is_king piece || Piece.is_pawn piece

let en_passant board piece old_pos new_pos prev_move =
  match prev_move with
  | (Some (r1, c1) as old_position), (Some (r2, c2) as new_position) ->
      let piece_en_passant = get_piece board new_position in
      Piece.is_pawn piece
      && Piece.is_pawn piece_en_passant
      && Validate.is_in_en_passant old_position new_position
           (Piece.is_white piece)
      && Validate.valid_pawn_attack old_pos new_pos (Piece.is_white piece)
      && Validate.valid_en_passant old_pos new_pos new_position
  | _, _ -> failwith "Impossible: [prev_move] is not a move."

let legal_pawn board piece old_pos new_pos attacking =
  if Piece.is_pawn piece then
    if attacking then
      Validate.valid_pawn_attack old_pos new_pos (Piece.is_white piece)
    else if not attacking then
      if
        Piece.is_first_move piece
        && Validate.valid_pawn_dj_move old_pos new_pos (Piece.is_white piece)
        && not (clear_vertical board old_pos new_pos)
      then
        raise
          (InvalidMove
             "[legal_pawn] error: attempting to move a pawn at [new_pos] 2 \
              squares over another piece.")
      else
        Validate.valid_pawn_move old_pos new_pos
          (Piece.is_first_move piece)
          (Piece.is_white piece)
    else false
  else false

let move_piece board (old_pos : (char * int) option)
    (new_pos : (char * int) option) (castle : bool) (ai : bool)
    (prev_move : (char * int) option * (char * int) option) : board =
  let piece = get_piece board old_pos in
  if en_passant board piece old_pos new_pos prev_move then
    let piece' = Piece.move_piece piece new_pos in
    let piece_en_passant = get_piece board (snd prev_move) in
    let piece_en_passant_updated = Piece.capture_piece piece_en_passant in
    let board' = add_piece board piece' in
    let board'' = remove_piece board' piece in
    let board''' = remove_piece board'' piece_en_passant in
    add_piece board''' piece_en_passant_updated
  else
    let captured_piece = get_captured_piece board piece new_pos in
    let legal =
      if Piece.is_pawn piece then
        legal_pawn board piece old_pos new_pos (List.length captured_piece = 1)
      else
        castle
        || Piece.valid_move piece new_pos
           && clear_path board old_pos piece new_pos
    in
    if legal then
      let piece' = Piece.move_piece piece new_pos in
      if List.length captured_piece = 1 then
        let captured_piece = List.nth captured_piece 0 in
        let captured_piece_updated = Piece.capture_piece captured_piece in
        let board' = remove_piece board captured_piece in
        let board'' = add_piece board' captured_piece_updated in
        let board''' = remove_piece board'' piece in
        if Piece.should_promote piece' then
          add_piece board''' (Piece.promote_pawn piece' ai)
        else add_piece board''' piece'
      else if Piece.should_promote piece' then
        add_piece (remove_piece board piece) (Piece.promote_pawn piece' ai)
      else add_piece (remove_piece board piece) piece'
    else raise (InvalidMove "[move_piece] error: attempting illegal move.")

let castle (board : Piece.piece list) (old_pos : (char * int) option)
    (new_pos : (char * int) option) prev_move : board =
  let piece = get_piece board old_pos in
  let fm_king = Piece.is_first_move piece in
  match new_pos with
  | None -> failwith "Impossible: castling to a new position that is [None]. "
  | Some (x, y) ->
      if x = 'h' && y < 4 then
        (* White Queenside *)
        let rook = get_piece board (Some ('h', 1)) in
        let fm_rook = Piece.is_first_move rook in
        if fm_king && fm_rook then
          let board' = move_piece board old_pos new_pos true false prev_move in
          move_piece board' (Some ('h', 1)) (Some ('h', 4)) true false prev_move
        else
          raise
            (InvalidMove
               "[castle] error: attempting white queen side castle on King or \
                Rook which has already made it's first move.")
      else if x = 'h' && y > 4 then
        (* White Kingside *)
        let rook = get_piece board (Some ('h', 8)) in
        let fm_rook = Piece.is_first_move rook in
        if fm_king && fm_rook then
          let board' = move_piece board old_pos new_pos true false prev_move in
          move_piece board' (Some ('h', 8)) (Some ('h', 6)) true false prev_move
        else
          raise
            (InvalidMove
               "[castle] error: attempting white king side castle on King or \
                Rook which has already made it's first move.")
      else if x = 'a' && y < 4 then
        (* Black Queenside *)
        let rook = get_piece board (Some ('a', 1)) in
        let fm_rook = Piece.is_first_move rook in
        if fm_king && fm_rook then
          let board' = move_piece board old_pos new_pos true false prev_move in
          move_piece board' (Some ('a', 1)) (Some ('a', 4)) true false prev_move
        else
          raise
            (InvalidMove
               "[castle] error: attempting black queen side castle on King or \
                Rook which has already made it's first move.")
      else if x = 'a' && y > 4 then
        (* Black Kingside *)
        let rook = get_piece board (Some ('a', 8)) in
        let fm_rook = Piece.is_first_move rook in
        if fm_king && fm_rook then
          let board' = move_piece board old_pos new_pos true false prev_move in
          move_piece board' (Some ('a', 8)) (Some ('a', 6)) true false prev_move
        else
          raise
            (InvalidMove
               "[castle] error: attempting black king side castle on King or \
                Rook which has already made it's first move.")
      else
        raise
          (InvalidMove "[castle] error: attempting castle invalid [new_pos].")
