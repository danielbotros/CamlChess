type color
type piece_type
type piece

val piece_to_string : piece_type -> string
val string_to_piece : string -> piece_type
val create_piece : string -> (char * int) option -> color -> piece
val get_piece_type : piece -> piece_type
val get_position : piece -> (char * int) option
val get_color : piece -> color
val is_first_move : piece -> bool
val same_pos : piece -> piece -> bool
val capture_piece : piece -> piece -> piece
val move_piece : piece -> (char * int) option -> piece
val valid_pos : char * int -> bool
val pos_of_string : string -> (char * int) option
val string_of_pos : (char * int) option -> string
val valid_pawn_move : piece -> (char * int) option -> bool
val valid_knight_move : piece -> (char * int) option -> bool
val valid_king_move : piece -> (char * int) option -> bool
val valid_queen_move : piece -> (char * int) option -> bool
val valid_rook_move : piece -> (char * int) option -> bool
val valid_bishop_move : piece -> (char * int) option -> bool
