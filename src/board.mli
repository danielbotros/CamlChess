type board

exception InvalidMove
(** Raised when there is an invalid move requested of any function*)

val init_board : string list list -> board
(** [init_board board] is the board represented by the current board displayed
    on the GUI. *)

val get_pieces : board -> Piece.piece list
(** [get_pieces board] returns all of the pieces on the board **)

val board_to_list : board -> string list list
(** [board_to_list lst] is the GUI represenation of the current board. *)

val empty : board
(** The empty board. *)

val get_piece : board -> (char * int) option -> Piece.piece
(** [get_piece board pos] is the piece at [pos]. *)

val get_pieces : board -> Piece.piece list

val remove_piece : board -> Piece.piece -> board
(** [remove_piece voard piece] is the updated board after removing [piece]. *)

val add_piece : board -> Piece.piece -> board
(** [add_piece voard piece] is the updated board after adding [piece]. *)

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

val move :
  board ->
  (char * int) option ->
  (char * int) option ->
  bool ->
  (char * int) option * (char * int) option ->
  board
(** [move board old_pos new_pos castle prev_move] is the updated board after
    moving the piece at [old_pos] to [new_pos], capturing or castling if
    applicable. Raises: [InvalidMove] if this move is not on board, not legal
    for the piece, or not legal in the rules of chess. *)

val graveyard : board -> string list
(** [graveyard board] is the graveyard list. *)

val castle :
  board ->
  (char * int) option ->
  (char * int) option ->
  (char * int) option * (char * int) option ->
  board
(** [move board old_pos new_pos] is the updated board after moving the piece at
    [old_pos] to [new_pos], capturing if applicable. Raises: [InvalidMove] if
    this move is not on board, not legal for the piece, or not legal in the
    rules of chess. *)
