type state

val board : state -> Board.board
val checkmate : state -> bool
val stalemate : state -> bool
val move : char * int -> char * int -> state -> state
