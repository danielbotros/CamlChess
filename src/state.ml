type state = {
  board : Board.board;
  graveyard : string list;
  past_moves : ((char * int) option * (char * int) option) list;
  past_moves_pieces : string list;
  turn : int;
}

exception CheckMate
exception StaleMate
exception Check

let get_board st = st.board
let get_turn s = s.turn
let get_color p = p |> Piece.get_color |> Piece.color_to_string

let get_past_moves st =
  ( st.past_moves_pieces,
    st.past_moves |> List.map (fun (x, y) -> (Option.get x, Option.get y)) )

let get_pos pos =
  match pos with
  | None -> failwith "Impossible: get_pos called on [None] position. "
  | Some (c, i) -> (c, i)

let print_pos = function
  | None -> ()
  | Some (x, y) -> print_endline (String.make 1 x ^ "," ^ string_of_int y)

let create_state lst =
  {
    board = lst;
    graveyard = [];
    past_moves = [];
    past_moves_pieces = [];
    turn = 1;
  }

let board st = Board.board_to_list st.board
let board_info st moves = Board.board_info_to_list st.board moves
let graveyard st = Board.graveyard st.board

let rec valid_turn { board; graveyard; past_moves; turn } pos =
  let color = if turn mod 2 = 1 then "White" else "Black" in
  valid_pos board color pos

and valid_pos board color pos =
  let piece_color = Board.get_piece board pos |> get_color in
  piece_color = color

let possible_moves st (pos : char * int) =
  let pos' = ref pos in
  let x = fst !pos' in
  let y = snd !pos' in
  let moves = ref [] in
  for row = 1 to 8 do
    for col = 1 to 8 do
      try
        let _ =
          Board.move_piece st.board
            (Some (x, y))
            (Some (char_of_int (row + 96), col))
            false true
            (try List.hd st.past_moves
             with Failure e -> (Some ('h', 1), Some ('h', 8)))
        in
        moves := Some (char_of_int (row + 96), col) :: !moves
      with _ -> moves := None :: !moves
    done
  done;
  !moves

let all_moves st =
  List.map
    (fun piece ->
      let pos = Piece.get_position piece in
      if get_turn st mod 2 = 1 then
        if pos <> None && Piece.is_white piece then
          possible_moves st (get_pos pos)
        else [ None ]
      else if pos <> None && Piece.is_black piece then
        possible_moves st (get_pos pos)
      else [ None ])
    (Board.get_pieces (get_board st))

let move (castle : bool) (ai : bool) st old_pos new_pos =
  if valid_turn st old_pos then
    {
      board =
        (if castle then
         Board.castle st.board old_pos new_pos
           (try List.hd st.past_moves
            with Failure e -> (Some ('h', 1), Some ('h', 8)))
        else
          Board.move_piece st.board old_pos new_pos false ai
            (try List.hd st.past_moves
             with Failure e -> (Some ('h', 1), Some ('h', 8))));
      graveyard = Board.graveyard st.board;
      past_moves = (old_pos, new_pos) :: st.past_moves;
      past_moves_pieces =
        (if st.turn mod 2 = 1 then
         if (st.turn + 1) / 2 >= 10 then
           (string_of_int ((st.turn + 1) / 2)
           ^ ". "
           ^ (old_pos |> Board.get_piece st.board |> Piece.piece_to_string))
           :: st.past_moves_pieces
         else
           (string_of_int ((st.turn + 1) / 2)
           ^ ".  "
           ^ (old_pos |> Board.get_piece st.board |> Piece.piece_to_string))
           :: st.past_moves_pieces
        else
          (old_pos |> Board.get_piece st.board |> Piece.piece_to_string)
          :: st.past_moves_pieces);
      turn = st.turn + 1;
    }
  else failwith "[move] error: Not this team's turn. "

let get_all_states st =
  let board = Board.get_pieces (get_board st) in
  let states =
    List.flatten
      (List.map2
         (fun moves_list piece ->
           let piece_position = Piece.get_position piece in
           if piece_position <> None then
             List.map
               (fun a_move ->
                 if a_move <> None then move false true st piece_position a_move
                 else st)
               moves_list
           else [ st ])
         (all_moves st) board)
  in
  List.filter (fun state -> state <> st) states

let find_king st color =
  List.find
    (fun p -> get_color p = color && Piece.is_king p)
    (Board.get_pieces st.board)
  |> Piece.get_position

let can_be_captured st =
  let color = if st.turn mod 2 = 0 then "Black" else "White" in
  let king_pos = find_king st color in
  let pieces = List.map Piece.get_position (Board.get_pieces st.board) in
  List.exists
    (fun piece_pos ->
      try
        let _ =
          Board.move_piece st.board piece_pos king_pos false true
            (try List.hd st.past_moves
             with Failure e -> (Some ('h', 1), Some ('h', 8)))
        in
        true
      with _ -> false)
    pieces

let no_moves st =
  let states = get_all_states st in
  let s =
    List.filter
      (fun s -> not (can_be_captured { s with turn = st.turn }))
      states
  in
  s = []

let update_state (castle : bool) (ai : bool) st old_pos new_pos =
  let new_state = move castle ai st old_pos new_pos in
  let no_legal_moves = no_moves new_state in
  let in_check = can_be_captured new_state in
  if can_be_captured { new_state with turn = st.turn } then raise Check
  else if no_legal_moves && in_check then raise CheckMate
  else if no_legal_moves then raise StaleMate
  else new_state
