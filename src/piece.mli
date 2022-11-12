type color
(** The type [color] represents the color of the piece, either [Black] or
    [White]. *)

type piece_type
(** The type [piece_type] represents the type of the piece, either [King],
    [Queen], [Rook], [Bishop], [Pawn], or [Knight]. *)

type piece
(** The type [piece] represents a piece and the information relevant to it, such
    as it's position, piece type, color, and whether or not it is the piece's
    first move. *)

val string_to_color : string -> color
(** [string_to_color str] is the color that [str] represents. Raises: [Failure]
    if [str] does not represent a valid color.*)

val color_to_string : color -> string
(** [color_to_string color] is the string represenation of [color]*)

val piece_to_string : piece -> string
(** [piece_to_string piece] is string represenation of [piece]. *)

val string_to_piece : string -> piece_type * color
(** [string_to_piece str] is the piece that [str] represents. Raises: [Failure]
    if [str] does not represent a valid piece. *)

val create_piece : piece_type -> (char * int) option -> color -> piece
(** [create_piece p_type pos col] is the [piece] with [p_type] piece type, [pos]
    position, and [col] color. *)

val get_piece_type : piece -> piece_type
(** [get_piece_type piece] is the piece type of [piece]. *)

val get_position : piece -> (char * int) option
(** [get_position piece] is the position of [piece]. *)

val get_color : piece -> color
(** [get_color piece] is the color of [piece]. s*)

val is_first_move : piece -> bool
(** [is_first_move piece] is true if it is the first move of [piece], false
    otherwise. *)

val is_pawn : piece -> bool
(** [is_pawn piece] is true if [piece] is a pawn, false otherwise. *)

val is_king : piece -> bool
(** [is_king piece] is true if [piece] is a king, false otherwise. *)

val is_queen : piece -> bool
(** [is_queen piece] is true if [piece] is a queen, false otherwise. *)

val is_knight : piece -> bool
(** [is_knight piece] is true if [piece] is a knight, false otherwise. *)

val is_bishop : piece -> bool
(** [is_bishop piece] is true if [piece] is a bishop, false otherwise. *)

val is_rook : piece -> bool
(** [is_pawn rook] is true if [piece] is a rook, false otherwise. *)

val capture_piece : piece -> piece
(** [capture_piece piece] is [piece] with a captured position. *)

val move_piece : piece -> (char * int) option -> piece
(** [move_piece piece pos] is [piece] with it's updated postiion at [pos]. *)

val valid_move : piece -> (char * int) option -> bool
(** [valid_move piece pos] is true if moving [piece] to [pos] is valid, false
    otherwise. *)
