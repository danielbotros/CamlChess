(** Functions and their helpers for mass state generation and evaluation
    calculations for the AI. *)

val difficulty : int
(** [difficulty] determines the difficult / intelligence of the AI.
    [difficulty]+2 is the number of moves ahead the AI will think. It's default
    value is 2. *)

val white_pawns : State.state -> float
(** [white_pawns st] is the number of white pawns in this [st]. *)

val black_pawns : State.state -> float
(** [black_pawns st] is the number of black pawns in this [st]. *)

val white_knights : State.state -> float
(** [white_knights st] is the number of white knight in this [st]. *)

val black_knights : State.state -> float
(** [black_knightss st] is the number of black knights in this [st]. *)

val white_bishops : State.state -> float
(** [white_bishops st] is the number of white bishops in this [st]. *)

val black_bishops : State.state -> float
(** [black_bishops st] is the number of black bishops in this [st]. *)

val white_rooks : State.state -> float
(** [white_rooks st] is the number of white rooks in this [st]. *)

val black_rooks : State.state -> float
(** [black_rooks st] is the number of black rooks in this [st]. *)

val white_kings : State.state -> float
(** [white_kings st] is the number of white kings in this [st]. *)

val black_kings : State.state -> float
(** [black_kings st] is the number of black kings in this [st]. *)

val white_queens : State.state -> float
(** [white_queens st] is the number of white queens in this [st]. *)

val black_queens : State.state -> float
(** [black_queens st] is the number of black queens in this [st]. *)

val evaluate : State.state -> float
(** [evaluate st] is the evaluation function / value for this state.*)

val lst_max : 'a list -> 'a
(** [lst_max lst] is the maximum value of this [lst]. *)

val accumlate_score : State.state -> int -> float
(** [accumlate_score st depth] is the score of the best possible set of moves
    (i.e. game states) after [depth]+1 moves. *)

val optimal_state : State.state -> State.state
(** [optimal_state st] is the best move that the AI can be make from the current
    state [st]. *)
