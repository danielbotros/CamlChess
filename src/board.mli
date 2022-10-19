type board

val init_board : string list -> board
val current_pieces : board -> Piece.piece list
val valid_move : char * int -> char * int -> board -> bool
val move : char * int -> char * int -> board -> board
