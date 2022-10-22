type color
type piece_type
type piece

val piece_to_string : piece -> string
val string_to_piece : string -> piece_type * color
val create_piece : piece_type -> (char * int) option -> color -> piece
val string_to_color : string -> color
val get_piece_type : piece -> piece_type
val get_position : piece -> (char * int) option
val get_color : piece -> color
val is_first_move : piece -> bool
val is_pawn : piece -> bool
val is_king : piece -> bool
val is_queen : piece -> bool
val is_knight : piece -> bool
val is_bishop : piece -> bool
val is_rook : piece -> bool
val capture_piece : piece -> piece -> piece
val move_piece : piece -> (char * int) option -> piece
val valid_move : piece -> (char * int) option -> bool