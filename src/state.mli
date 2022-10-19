type t
(** The abstract type representing the game state*)

val init_state : Board.t -> t
(** [init_state a] is the initial state of the game when playing chess [a]. *)