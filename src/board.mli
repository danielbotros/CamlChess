type board

val init_board : string list list -> board

val board_to_list : board -> string list list
(** val valid_move : char * int -> char * int -> board -> bool val move : char *
    int -> char * int -> board -> board val get_piece : char * int ->
    Piece.piece *)
