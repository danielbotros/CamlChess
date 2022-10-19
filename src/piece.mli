type piece

val get_piece_type : piece -> string
val get_position : piece -> char * int
val get_color : piece -> string
val change_pos : piece -> char * int -> piece
val valid_move : piece -> char * int -> bool
