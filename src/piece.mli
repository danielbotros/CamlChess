type piece

val create_piece : string -> (char * int) option -> string -> piece
(**
val get_player : piece -> int
val change_pos : piece -> char * int -> piece
val valid_move : piece -> char * int -> bool
    *)
val get_piece_type : piece -> string
val get_position : piece -> (char * int) option
val get_color : piece -> string
