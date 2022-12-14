let difficulty = 2

let white_pawns st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_pawn piece && Piece.is_white piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let black_pawns st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_pawn piece && Piece.is_black piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let white_knights st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_knight piece && Piece.is_white piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let black_knights st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_knight piece && Piece.is_black piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let black_bishops st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_bishop piece && Piece.is_black piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let white_bishops st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_bishop piece && Piece.is_white piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let black_rooks st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_rook piece && Piece.is_black piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let white_rooks st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_rook piece && Piece.is_white piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let white_kings st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_king piece && Piece.is_white piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let black_kings st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_king piece && Piece.is_black piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let black_queens st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_queen piece && Piece.is_black piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let white_queens st =
  List.fold_left
    (fun acc piece ->
      if Piece.is_queen piece && Piece.is_white piece then acc +. 1. else acc)
    0.
    (Board.get_pieces (State.get_board st))

let evaluate st =
  (200. *. (black_kings st -. white_kings st))
  +. (9. *. (black_queens st -. white_queens st))
  +. (5. *. (black_rooks st -. white_rooks st))
  +. (3. *. (black_knights st -. white_knights st))
  +. (3. *. (black_bishops st -. white_bishops st))
  +. (1. *. (black_pawns st -. white_pawns st))

let lst_max lst = List.hd (List.rev (List.sort compare lst))

let rec traverse_tree state depth =
  lst_max
    (List.map
       (fun next_state ->
         if depth = 0 then evaluate next_state
         else traverse_tree next_state (depth - 1))
       (State.get_all_states state))

let () = Random.self_init ()

let optimal_state st =
  let next_moves = State.get_all_states st in
  let os =
    List.map
      (fun next_move ->
        (evaluate next_move +. traverse_tree next_move difficulty, next_move))
      next_moves
  in
  let os_score = lst_max (List.map (fun (score, _) -> score) os) in
  let best_states = List.filter (fun (score, _) -> score = os_score) os in
  snd (List.nth best_states (Random.int (List.length best_states)))
