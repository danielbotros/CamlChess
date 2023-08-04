(** An instance of the game Chess. This module stores an instance of the game
    Chess, defines functions for the setters/getters of a state's information,
    GUI compatible abstractions, future state/move generation, as well as the
    functions for determining Checkmate, Stalemate, and Check. *)

type state
(** The type [state] represents a game state and the information relevant to it,
    such as the board, graveyard, a list of past moves, and current turn. *)

exception CheckMate
exception StaleMate
exception Check

val get_board : state -> Board.board
(** [get_board st] returns the state's board. *)

val get_turn : state -> int
(** [get_turn st] returns the integer representing current turn. *)

val get_color : Piece.piece -> string
(** [get_color p] is the string represenation of the color of [p]. *)

val get_past_moves : state -> string list * ((char * int) * (char * int)) list
(** [get_past_moves st] returns a pair of the list of pieces moved at each turn
    (such as "1. ♙") as strings along with the list of past moves (as in the
    coordinates) by both players. *)

val get_pos : (char * int) option -> char * int
(** [get_pos pos] is the unwrapped value of [pos] from it's option. *)

val print_pos : (char * int) option -> unit
(** [print_pos] is the this position printed. *)

val create_state : Board.board -> state
(** [create_state board] initializes a game with starting board.*)

val board : state -> string list list
(** [board st] is the GUI representation of the state's board. *)

val board_info : state -> (char * int) option list -> string list list
(** [board_info st moves ] is the GUI representation of the state's board with
    all possible moves for a piece at it's current location highlighted. *)

val graveyard : state -> string list
(** [graveyard st] is the GUI represenation of the state's graveyard. *)

val valid_turn : state -> (char * int) option -> bool
(** [valid_turn st pos] is true if it is the turn of the piece at position
    [pos], false otherwise. *)

val valid_pos : Board.board -> string -> (char * int) option -> bool
(** [valid_pos board color pos] is a helper function for [valid_turn] that
    verifies the [color] of the team who's turn it is and the color of the piece
    at position [pos]. *)

val possible_moves : state -> char * int -> (char * int) option list
(** [possible_moves st pos] is all the possible moves a piece can make from
    [pos]. *)

val all_moves : state -> (char * int) option list list
(** [all_moves st] is the 2D list of all possible moves from this [state] with
    each list representing all possible moves for a piece from it's current
    position. *)

val move :
  bool -> bool -> state -> (char * int) option -> (char * int) option -> state
(** [move castle ai st old_pos new_pos] is the state with the piece at position
    [old_pos] moved to [new_pos], capturing / castling as necessary. *)

val get_all_states : state -> state list
(** [get_all_states st] is all the possible legal game states that can be
    derived from [st]. *)

val find_king : state -> string -> (char * int) option
(** [find_king st color] is the [color] King of this [st]. *)

val can_be_captured : state -> bool
(** [can_be_captured st] is true if the King of this [st]'s turn color can
    currently be captured by any piece of the opposing team on the board. *)

val no_moves : state -> bool
(** [no_moves st] is true if neither team has any more legal moves to make from
    this [st], false otherwise. *)

val update_state :
  bool -> bool -> state -> (char * int) option -> (char * int) option -> state
(** [update_state castle ai state old_pos new_pos] updates the game state after
    one player turn's actions, which may throw [Check], [Checkmate], or
    [Stalemate] *)
