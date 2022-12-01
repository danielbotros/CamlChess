type state = {
  board : Board.board;
  graveyard : string;
  past_moves : (char * int) option list;
  turn : int;
}

let board st = Board.board_to_list st.board
let graveyard st = Board.graveyard st.board

let create_state lst =
  { board = lst; graveyard = ""; past_moves = []; turn = 1 }

let update_state st (old_pos : (char * int) option)
    (new_pos : (char * int) option) =
  {
    board = Board.move st.board old_pos new_pos;
    graveyard = Board.graveyard st.board;
    past_moves = new_pos :: st.past_moves;
    turn = st.turn mod 2;
  }
