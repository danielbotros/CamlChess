type state = {
  board : Board.board;
  graveyard : string list;
  past_moves : (char * int) option list;
  turn : int;
}

let get_color p = p |> Piece.get_color |> Piece.color_to_string

let rec valid_move { board; past_moves; turn } pos =
  let color = if turn mod 2 = 0 then "White" else "Black" in
  valid_pos board color pos

and valid_pos board color pos =
  let piece_color = Board.get_piece board pos |> get_color in
  piece_color = color

and checkmate board color =
  let _ = valid_king_moves board color in
  ()

and valid_king_moves board color =
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
          Board.move board (Some (x, y)) (Some (char_of_int (row + 96), col))
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

and check_opponent king_moves board color =
  let pieces =
    List.map (fun p -> Piece.get_position p) (Board.get_pieces board)
  in
  List.exists
    (fun king_pos ->
      not
        (List.exists
           (fun piece_pos ->
             try
               let _ = Board.move board piece_pos king_pos in
               true
             with _ -> false)
           pieces))
    king_moves

let board st = Board.board_to_list st.board
let graveyard st = Board.graveyard st.board

let create_state lst =
  { board = lst; graveyard = []; past_moves = []; turn = 1 }

let update_state st (old_pos : (char * int) option)
    (new_pos : (char * int) option) =
  if valid_move st old_pos then
    {
      board = Board.move st.board old_pos new_pos;
      graveyard = Board.graveyard st.board;
      past_moves = new_pos :: st.past_moves;
      turn = st.turn + 1;
    }
  else failwith "invalid move"
