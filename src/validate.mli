(** Functions and their helpers for move checking validation of piece types and
    path clearing. *)

val char_to_int : char -> int
(** [char_to_int c] is the integer representation of [c], a column coordinate,
    in chess. *)

val pos_of_string : string -> (char * int) option
(** [pos_of_string str] is the position represented by [str]. *)

val string_of_pos : (char * int) option -> string
(** [string_of_pos pos] is the string represenation of [pos]. Raises: [Failure]
    is [pos] is not a valid position. *)

val same_pos : (char * int) option -> (char * int) option -> bool
(** [same_pos pos1 pos2] is true if [pos1] and [pos2] are the same position,
    false otherwise. *)

val valid_pos : char * int -> bool
(** [valid_pos pos] is true if [pos] is a position on the board, false
    otherwise. *)

val vertical_move : (char * int) option -> (char * int) option -> bool
(** [vertical_move pos1 pos2] is true if moving from [pos1] to [pos2] can be
    done in one vertical move, false otherwise. *)

val horizontal_move : (char * int) option -> (char * int) option -> bool
(** [horiztonal_move pos1 pos2] is true if moving from [pos1] to [pos2] can be
    done in one horizontal move, false otherwise. *)

val diagonal_move : (char * int) option -> (char * int) option -> bool
(** [diagonal_move pos1 pos2] is true if moving from [pos1] to [pos2] can be
    done in one diagonal move, false otherwise. *)

val is_in_en_passant :
  (char * int) option -> (char * int) option -> bool -> bool
(** [is_in_en_passant pos1 pos2 white] is true if moving from [pos1] to [pos2]
    put the pawn at [pos2] in risk of en passant, false if otherwise.*)

val valid_en_passant :
  (char * int) option -> (char * int) option -> (char * int) option -> bool
(** [valid_en_passant pos1 pos2 pos3] is true if moving from [pos1] to [pos2] is
    an en passant capture for the pawn at [pos3], false if otherwise. *)

val valid_pawn_attack :
  (char * int) option -> (char * int) option -> bool -> bool
(** [valid_pawn_attack pos1 pos2 white] is true if moving from [pos1] to [pos2]
    is a legal attacking move for a pawn, false otherwise. *)

val valid_pawn_dj_move :
  (char * int) option -> (char * int) option -> bool -> bool
(** [valid_pawn_dj_move pos1 pos2 white] is true if moving from [pos1] to [pos2]
    is a legal double jump move for a pawn, false otherwise. *)

val valid_pawn_move :
  (char * int) option -> (char * int) option -> bool -> bool -> bool
(** [valid_pawn_move_white pos1 pos2 first_move white] is true if moving from
    [pos1] to [pos2] is a legal move for a pawn, it is not attacking, false
    otherwise. *)

val valid_castle : (char * int) option -> (char * int) option -> bool
(** [valid_castle old_pos new_pos] is true if moving from [old_pos] to [new_pos]
    is a legal castling move for a king, false otherwise. *)

val valid_knight_move : (char * int) option -> (char * int) option -> bool
(** [valid_knight_move pos1 pos2] is true if moving from [pos1] to [pos2] is a
    legal move for a knight, false otherwise. *)

val valid_king_move : (char * int) option -> (char * int) option -> bool
(** [valid_king_move pos1 pos2] is true if moving from [pos1] to [pos2] is a
    legal move for a king, false otherwise. *)

val valid_queen_move : (char * int) option -> (char * int) option -> bool
(** [valid_queen_move pos1 pos2] is true if moving from [pos1] to [pos2] is a
    legal move for a queen, false otherwise. *)

val valid_rook_move : (char * int) option -> (char * int) option -> bool
(** [valid_rook_move pos1 pos2] is true if moving from [pos1] to [pos2] is a
    legal move for a rook, false otherwise. *)

val valid_bishop_move : (char * int) option -> (char * int) option -> bool
(** [valid_bishop_move pos1 pos2] is true if moving from [pos1] to [pos2] is a
    legal move for a bishop, false otherwise. *)

val is_in_vertical_path :
  (char * int) option -> (char * int) option -> (char * int) option -> bool
(** [is_in_vertical_path pos1 pos2 pos3] is true if [pos2] is in a vertical path
    between [pos1] and [pos3], false otherwise. *)

val is_in_horizontal_path :
  (char * int) option -> (char * int) option -> (char * int) option -> bool
(** [is_in_horizontal_path pos1 pos2 pos3] is true if [pos2] is in a horizontal
    path between [pos1] and [pos3], false otherwise. *)

val is_in_diagonal_path :
  (char * int) option -> (char * int) option -> (char * int) option -> bool
(** [is_in_diagonal_path pos1 pos2 pos3] is true if [pos2] is in a diagonal path
    between [pos1] and [pos3], false otherwise. *)
