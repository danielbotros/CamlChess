type state

val board : state -> string list list
val checkmate : state -> bool
val stalemate : state -> bool
val move : char * int -> char * int -> state -> state
val create_state : Board.board -> state
