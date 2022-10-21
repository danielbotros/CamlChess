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
  match piece with
  | Pawn -> "p"
  | Knight -> "n"
  | King -> "k"
  | Queen -> "q"
  | Rook -> "r"
  | Bishop -> "b"

let string_to_piece str =
  match str with
  | "p" -> Pawn
  | "n" -> Knight
  | "k" -> King
  | "q" -> Queen
  | "r" -> Rook
  | "b" -> Bishop
  | _ -> failwith "invalid piece_type"

let char_to_int c = Char.code c - Char.code '0'

let create_piece p_type pos col =
  { piece_type = p_type; position = pos; color = col; first_move = true }

let get_piece_type piece = piece.piece_type
let get_position piece = piece.position
let get_color piece = piece.color
let is_first_move piece = piece.first_move

let same_pos piece1 piece2 =
  match (piece1.position, piece2.position) with
  | Some (c1, i1), Some (c2, i2) -> c1 = c2 && i1 = i2
  | _, _ -> false

let capture_piece piece attacking_piece =
  if same_pos piece attacking_piece then { piece with position = None }
  else piece

let valid_pos pos =
  match pos with
  | c, i ->
      Char.code 'a' <= Char.code c
      && Char.code c <= Char.code 'h'
      && i < 8 && i > 1
  | _ -> false

let move_piece piece pos =
  match pos with
  | Some p -> { piece with position = Some p; first_move = false }
  | None -> { piece with position = None; first_move = false }

let pos_of_string str1 : position =
  Some (str1.[0], int_of_char str1.[1] - int_of_char '0')

let string_of_pos pos =
  match pos with
  | Some (c, i) -> Some (String.make 1 c ^ string_of_int i)
  | None -> None

let valid_pawn_move piece pos =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
      || (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 2
         && piece.first_move = true
  | _ -> false

let valid_knight_move piece pos =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 2
      && c1 - c2 |> abs = 1
      || (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
         && c1 - c2 |> abs = 2
  | _ -> false

let valid_king_move piece pos =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
      && c1 - c2 |> abs = 1
      || c1 - c2 |> abs = 1
      || (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
  | _ -> false

let valid_queen_move piece pos =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = (c2 - c1 |> abs)
      || valid_pos ((r2 |> char_to_int) - (r1 |> char_to_int) |> char_of_int, c2)
         && c2 - c1 = 0
      || valid_pos (r2, c2 - c1 |> abs)
         && (r2 |> char_to_int) - (r1 |> char_to_int) = 0
  | _ -> false

let valid_rook_move piece pos =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      valid_pos ((r2 |> char_to_int) - (r1 |> char_to_int) |> char_of_int, c2)
      && c2 - c1 = 0
      || valid_pos (r2, c2 - c1 |> abs)
         && (r2 |> char_to_int) - (r1 |> char_to_int) = 0
  | _ -> false

let valid_bishop_move piece pos =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = (c2 - c1 |> abs)
  | _ -> false
