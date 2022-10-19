type color =
  | White
  | Black

type piece_types =
  | Pawn
  | Knight
  | King
  | Queen
  | Rook
  | Bishop

let piece_to_string piece =
  match piece with
  | Pawn -> "pawn"
  | Knight -> "knight"
  | King -> "king"
  | Queen -> "queen"
  | Rook -> "rook"
  | Bishop -> "bishop"

let string_to_piece str =
  match str with
  | "pawn" -> Some Pawn
  | "knight" -> Some Knight
  | "king" -> Some King
  | "queen" -> Some Queen
  | "rook" -> Some Rook
  | "bishop" -> Some Bishop
  | _ -> None

type position = (char * int) option

type piece = {
  piece_type : piece_types;
  position : position;
  color : color;
  first_move : bool;
}

let create_piece p_type pos col =
  { piece_type = p_type; position = pos; color = col; first_move = true }

let get_piece piece = piece.piece_type
let get_position piece = piece.position
let get_color piece = piece.color

let same_pos piece1 piece2 =
  match (piece1.position, piece2.position) with
  | Some (c1, i1), Some (c2, i2) -> c1 = c2 && i1 = i2
  | _, _ -> false

let capture_piece piece attacking_piece =
  if same_pos piece attacking_piece then { piece with position = None }
  else piece

let valid_pos pos = match pos with
| (c, i) -> Char.code 'a' <= Char.code c && Char.code c <= Char.code 'z' && i < 8 && i > 1
| _ -> false
let move_piece piece pos = { piece with position = Some pos; first_move = false }

let string_to_move str = match str with 
| 
|
