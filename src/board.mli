(** An instance of a chess board. This module stores an instance of the board,
    defines functions for the setters/getters of a state's information, GUI
    compatible abstractions, clear path validation , as well as moving and
    capturing pieces. *)

type board

exception InvalidMove of string
(** Raised when there is an invalid move requested of any function. *)

val empty : board
(** The empty board. *)

val get_piece : board -> (char * int) option -> Piece.piece
(** [get_piece board pos] is the piece at [pos] on the board. *)

val get_pieces : board -> Piece.piece list
(** [get_pieces board] returns all of the pieces on the board. **)

val remove_piece : board -> Piece.piece -> board
(** [remove_piece board piece] is the updated board after removing [piece]. *)

val add_piece : board -> Piece.piece -> board
(** [add_piece voard piece] is the updated board after adding [piece]. *)

val graveyard : board -> string list
(** [graveyard board] is the graveyard list. *)

val get_captured_piece :
  board -> Piece.piece -> (char * int) option -> Piece.piece list
(** [get_captured_piece board piece new_pos] is the list of the piece at
    [new_pos], empty if there is no piece at [new_pos]. Raises [InvalidMove] if
    the piece at [new_pos] is on your team. *)

val init_board : string list list -> board
(** [init_board board] is the board represented by the current board displayed
    on the GUI. *)

val board_to_list : board -> string list list
(** [board_to_list lst] is the data represenation represenation of the current
    board. *)

val board_info_to_list : board -> (char * int) option list -> string list list
(** [board_info_to_list lst] is the data representation represenation of all the
    possible moves that a certain piece can make from it's current position. *)

val clear_vertical : board -> (char * int) option -> (char * int) option -> bool
(** [clear_vertical board new_pos old_pos] is true if there is a clear vertical
    path, meaning no pieces, between [old_pos] and [new_pos], false if otherwise *)

val clear_horizontal :
  board -> (char * int) option -> (char * int) option -> bool
(** [clear_horizontal board new_pos old_pos] is true if there is a clear
    horizontal path, meaning no pieces, between [old_pos] and [new_pos], false
    if otherwise *)

val clear_diagonal : board -> (char * int) option -> (char * int) option -> bool
(** [clear_diagonal board new_pos old_pos] is true if there is a clear diagonal
    path, meaning no pieces, between [old_pos] and [new_pos], false if otherwise *)

val clear_path :
  board -> (char * int) option -> Piece.piece -> (char * int) option -> bool
(** [clear_path board new_pos piece old_pos] is true if there is a clear path
    for [piece] to move between [old_pos] and [new_pos], false if otherwise *)

val en_passant :
  board ->
  Piece.piece ->
  (char * int) option ->
  (char * int) option ->
  (char * int) option * (char * int) option ->
  bool
(** [en_passant board piece old_pos new_pos prev_mode] is true is this move is a
    legal en passant, false if otherwise. *)

val legal_pawn :
  board ->
  Piece.piece ->
  (char * int) option ->
  (char * int) option ->
  bool ->
  bool
(** [legal_pawn board piece new_pos old_pos attacking] is true if moving from
    [new_pos] to [old_pos] is a legal pawn move or capture, false otherwise. *)

val move_piece :
  board ->
  (char * int) option ->
  (char * int) option ->
  bool ->
  bool ->
  (char * int) option * (char * int) option ->
  board
(** [move_piece board old_pos new_pos castle ai prev_move] is the updated board
    after moving the piece at [old_pos] to [new_pos], capturing or castling if
    applicable. Raises: [InvalidMove] if this move is not on board, not legal
    for the piece, or not legal in the rules of chess. *)

val castle :
  board ->
  (char * int) option ->
  (char * int) option ->
  (char * int) option * (char * int) option ->
  board
(** [move board old_pos new_pos prev_move] is the updated board after moving the
    piece at [old_pos] to [new_pos], capturing if applicable. Raises:
    [InvalidMove] if this move is not on board, not legal for the piece, or not
    legal in the rules of chess. *)
