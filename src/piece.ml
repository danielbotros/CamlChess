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

type position = (char * int) option

type piece = {
  piece_type : piece_types;
  position : position;
  color : color;
  first_move : bool;
}

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
  | "pawn" -> Pawn
  | "knight" -> Knight
  | "king" -> King
  | "queen" -> Queen
  | "rook" -> Rook
  | "bishop" -> Bishop
  | _ -> failwith "invalid piece_type"

let char_to_int c = Char.code c - Char.code '0'

let create_piece p_type pos col =
  {
    piece_type = string_to_piece p_type;
    position = pos;
    color = col;
    first_move = true;
  }

let get_piece piece = piece.piece_type
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
  { piece with position = Some pos; first_move = false }

let pos_of_string str1 : position =
  Some (str1.[0], int_of_char str1.[1] - int_of_char '0')

let string_of_pos (pos : position) =
  match pos with
  | Some (c, i) -> Some (String.make 1 c ^ string_of_int i)
  | None -> None

let valid_pawn_move (pos : position) piece =
  match (piece.position, pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
      || (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 2
         && piece.first_move = true
  | _ -> false