type state = {
  board : Board.board;
  past_moves : (Piece.piece * char * int) list;
  turn : string;
}

let board st = Board.board_to_list st.board

let move movef movet st =
  {
    st with
    board = Board.move movef movet st.board;
    past_moves = (Board.get_piece movef, fst movet, snd movet) :: st.past_moves;
  }
