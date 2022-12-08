type state = {
  board : Board.board;
  graveyard : string list;
  past_moves : ((char * int) option * (char * int) option) list;
  past_moves_pieces : string list;
  turn : int;
}

let get_turn s = s.turn
let get_color p = p |> Piece.get_color |> Piece.color_to_string

let get_past_moves st =
  ( st.past_moves_pieces,
    st.past_moves |> List.map (fun (x, y) -> (Option.get x, Option.get y)) )

let rec valid_move { board; graveyard; past_moves; turn } pos =
  let color = if turn mod 2 = 1 then "White" else "Black" in
  valid_pos board color pos

and valid_pos board color pos =
  let piece_color = Board.get_piece board pos |> get_color in
  piece_color = color

and checkmate st board color =
  let _ = valid_king_moves st board color in
  ()

and valid_king_moves st board color =
  let pos = ref (-1, -1) in
  for row = 1 to 8 do
    for col = 1 to 8 do
      try
        if
          Board.get_piece board (Some (char_of_int (row + 96), col))
          |> Piece.is_king
        then pos := (row, col)
      with _ -> ()
    done
  done;
  let x = char_of_int (fst !pos + 96) in
  let y = snd !pos in
  let moves = ref [] in
  for row = 1 to 8 do
    for col = 1 to 8 do
      try
        let _ =
          Board.move board
            (Some (x, y))
            (Some (char_of_int (row + 96), col))
            false
            (try List.hd st.past_moves
             with Failure e -> (Some ('h', 1), Some ('h', 8)))
        in
        moves := Some (char_of_int (row + 96), col) :: !moves
      with _ -> ()
    done
  done;
  List.iter
    (fun p ->
      match p with
      | Some (x, y) -> print_endline (String.make 1 x ^ " , " ^ string_of_int y)
      | None -> ())
    !moves;
  !moves

and check_opponent st king_moves board color =
  let pieces =
    List.map (fun p -> Piece.get_position p) (Board.get_pieces board)
  in
  List.exists
    (fun king_pos ->
      not
        (List.exists
           (fun piece_pos ->
             try
               let _ =
                 Board.move board piece_pos king_pos false
                   (try List.hd st.past_moves
                    with Failure e -> (Some ('h', 1), Some ('h', 8)))
               in
               true
             with _ -> false)
           pieces))
    king_moves

let board st = Board.board_to_list st.board
let get_board st = st.board
let graveyard st = Board.graveyard st.board

let create_state lst =
  {
    board = lst;
    graveyard = [];
    past_moves = [];
    past_moves_pieces = [];
    turn = 1;
  }

let update_state (castle : bool) st (old_pos : (char * int) option)
    (new_pos : (char * int) option) =
  if valid_move st old_pos then
    {
      board =
        (if castle then
         Board.castle st.board old_pos new_pos
           (try List.hd st.past_moves
            with Failure e -> (Some ('h', 1), Some ('h', 8)))
        else
          Board.move st.board old_pos new_pos false
            (try List.hd st.past_moves
             with Failure e -> (Some ('h', 1), Some ('h', 8))));
      graveyard = Board.graveyard st.board;
      past_moves = (old_pos, new_pos) :: st.past_moves;
      past_moves_pieces =
        (old_pos |> Board.get_piece st.board |> Piece.piece_to_string)
        :: st.past_moves_pieces;
      turn = st.turn + 1;
    }
  else failwith "invalid move"
