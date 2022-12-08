type state
(** The type [state] represents a game state and the information relevant to it,
    such as the board, a list of past moves, and current turn. *)

val board : state -> string list list
(** [board st] is the GUI representation of the state's board. *)

val get_board : state -> Board.board
(** [get_board st] returns the state's board. *)

val graveyard : state -> string list
(* [graveyard st] is the GUI represenation of the state's graveyard. *)

val create_state : Board.board -> state
(** [create state board] initializes a game with starting board.*)

val update_state :
  bool -> state -> (char * int) option -> (char * int) option -> state
(** [update state board old_pos new_pos] updates the game state after one player
    turn by moving a piece, adding the move to the move list, and changing turns*)

val get_turn : state -> int
(** [get_turn st] returns the integer representing current turn*)

val get_past_moves : state -> string list * ((char * int) * (char * int)) list
(** [get_past_moves st] returns a pair of the list of pieces moved (such as "♙")
    as strings along with the list of past moves (as in the coordinates) by both
    players*)
