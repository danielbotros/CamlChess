val same_pos : (char * int) option -> (char * int) option -> bool
val valid_pos : char * int -> bool
val pos_of_string : string -> (char * int) option
val string_of_pos : (char * int) option -> string option
val valid_pawn_attack : (char * int) option -> (char * int) option -> bool
val valid_pawn_move : (char * int) option -> (char * int) option -> bool
val valid_knight_move : (char * int) option -> (char * int) option -> bool
val valid_king_move : (char * int) option -> (char * int) option -> bool
val valid_queen_move : (char * int) option -> (char * int) option -> bool
val valid_rook_move : (char * int) option -> (char * int) option -> bool
val valid_bishop_move : (char * int) option -> (char * int) option -> bool

val is_in_vertical_path :
  (char * int) option -> (char * int) option -> (char * int) option -> bool

val is_in_horizontal_path :
  (char * int) option -> (char * int) option -> (char * int) option -> bool

val is_in_diagonal_path :
  (char * int) option -> (char * int) option -> (char * int) option -> bool
