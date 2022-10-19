type state

<<<<<<< Updated upstream
val board : state -> string list
val checkmate : state -> bool
val stalemate : state -> bool
val move : char * int -> char * int -> state -> state
=======
type t
(** The abstract type of values representing the game state*)

(* val init_state : Board.t -> t *)
(** [init_state board] is the initial state of the game. There are a bunch of
    chess pieces at certain positions, as well as captured pieces *)

(* val current_state : t -> string list list (** [current_state st] is the 2
   dimensional list of the all the chess pieces in their positions in state [st]
   *)

   (** The type representing the result of an attempted movement. *) type result
   = | Legal of t | Illegal

   (* val move : int * int -> Board.t -> int * int -> result *) (** [move start
   board end] is the result of attempting to move piece at position start to
   position end

   - If end is a valid end position, then the result is [Legal st'], where in
   [st'] the piece has been moved to a new location

   - Otherwise, the result is [Illegal].*) *)
>>>>>>> Stashed changes
