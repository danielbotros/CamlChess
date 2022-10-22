type color =
  | White
  | Black

let color_to_string color =
  match color with
  | White -> "White"
  | Black -> "Black"

let string_to_color color =
  match color with
  | "white" -> White
  | "black" -> Black
  | _ -> failwith "Invalid Color"

type piece_type =
  | Pawn
  | Knight
  | King
  | Queen
  | Rook
  | Bishop

type position = (char * int) option

type piece = {
  piece_type : piece_type;
  position : (char * int) option;
  color : color;
  first_move : bool;
}

let piece_to_string piece =
  match (piece.piece_type, piece.color) with
  | Pawn, Black -> "♙"
  | Pawn, White -> "♟"
  | Knight, Black -> "♘"
  | Knight, White -> "♞"
  | King, Black -> "♔"
  | King, White -> "♚"
  | Queen, Black -> "♛"
  | Queen, White -> "♕"
  | Rook, Black -> "♖"
  | Rook, White -> "♜"
  | Bishop, Black -> "♗"
  | Bishop, White -> "♝"

let string_to_piece str =
  match str with
  | "♙" -> (Pawn, Black)
  | "♟" -> (Pawn, White)
  | "♘" -> (Knight, Black)
  | "♞" -> (Knight, White)
  | "♔" -> (King, Black)
  | "♚" -> (King, White)
  | "♕" -> (Queen, White)
  | "♛" -> (Queen, Black)
  | "♖" -> (Rook, Black)
  | "♜" -> (Rook, White)
  | "♗" -> (Bishop, Black)
  | "♝" -> (Bishop, White)
  | _ -> failwith "invalid piece_type"

let create_piece p_type pos col =
  { piece_type = p_type; position = pos; color = col; first_move = true }

let get_piece_type piece = piece.piece_type
let get_position piece = piece.position
let get_color piece = piece.color
let is_first_move piece = piece.first_move
let is_pawn piece = get_piece_type piece = Pawn
let is_king piece = get_piece_type piece = King
let is_queen piece = get_piece_type piece = Queen
let is_knight piece = get_piece_type piece = Knight
let is_bishop piece = get_piece_type piece = Bishop
let is_rook piece = get_piece_type piece = Rook
let capture_piece piece attacking_piece = { piece with position = None }

let move_piece piece pos =
  match pos with
  | Some p -> { piece with position = Some p; first_move = false }
  | None -> { piece with position = None; first_move = false }

let valid_move piece pos =
  let old_pos = piece.position in
  match piece.piece_type with
  | Pawn -> Validate.valid_pawn_move old_pos pos
  | Knight -> Validate.valid_knight_move old_pos pos
  | King -> Validate.valid_king_move old_pos pos
  | Queen -> Validate.valid_queen_move old_pos pos
  | Rook -> Validate.valid_rook_move old_pos pos
  | Bishop -> Validate.valid_bishop_move old_pos pos
