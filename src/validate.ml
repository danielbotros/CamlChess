let char_to_int c = Char.code c - Char.code 'a'

let pos_of_string str : (char * int) option =
  Some (str.[0], int_of_char str.[1] - int_of_char '0')

let string_of_pos pos =
  match pos with
  | Some (c, i) -> String.make 1 c ^ string_of_int i
  | None -> failwith "Invalid pos"

let same_pos pos1 pos2 =
  match (pos1, pos2) with
  | Some (c1, i1), Some (c2, i2) -> c1 = c2 && i1 = i2
  | _, _ -> false

let valid_pos pos =
  match pos with
  | c, i ->
      Char.code 'a' <= Char.code c
      && Char.code c <= Char.code 'h'
      && i <= 8 && i >= 1

let vertical_move pos1 pos2 =
  match (pos2, pos1) with
  | Some (r1, c1), Some (r2, c2) -> c2 = c1
  | _ -> false

let horizontal_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) -> r2 = r1
  | _ -> false

let diagonal_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      c2 - c1 |> abs = ((r1 |> char_to_int) - (r2 |> char_to_int) |> abs)
  | _ -> false

let is_in_en_passant pos1 pos2 white =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      if white then (r2 |> char_to_int) - (r1 |> char_to_int) = 2 && c1 - c2 = 0
      else (r2 |> char_to_int) - (r1 |> char_to_int) = -2 && c1 - c2 = 0
  | _ -> false

let valid_en_passant pos1 pos2 pos3 =
  match (pos1, pos2, pos3) with
  | Some (r1, c1), Some (r2, c2), Some (r3, c3) -> r1 = r3 && c2 = c3
  | _ -> false

let valid_pawn_attack_white pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) = -1 && c1 - c2 |> abs = 1
  | _ -> false

let valid_pawn_attack_black pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) = 1 && c1 - c2 |> abs = 1
  | _ -> false

let valid_pawn_move_white pos1 pos2 first_move =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      ((r2 |> char_to_int) - (r1 |> char_to_int) = -1 && c1 - c2 |> abs = 0)
      || (r2 |> char_to_int) - (r1 |> char_to_int) = -2
         && c1 - c2 = 0
         && first_move
  | _ -> false

let valid_pawn_move_black pos1 pos2 first_move =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      ((r2 |> char_to_int) - (r1 |> char_to_int) = 1 && c1 - c2 |> abs = 0)
      || (r2 |> char_to_int) - (r1 |> char_to_int) = 2
         && c1 - c2 = 0
         && first_move
  | _ -> false

let valid_knight_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 2
      && c1 - c2 |> abs = 1
      || (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
         && c1 - c2 |> abs = 2
  | _ -> false

let valid_castle old_pos new_pos : bool =
  match (old_pos, new_pos) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 0 && c1 - c2 |> abs = 2
  | _ -> false

let valid_king_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
      && c1 - c2 |> abs = 1
      || c1 - c2 |> abs = 1
         && (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 0
      || (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = 1
         && c1 - c2 |> abs = 0
  | _ -> false

let valid_queen_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = (c2 - c1 |> abs)
      || valid_pos
           ( ((r2 |> char_to_int) - (r1 |> char_to_int) |> abs) + Char.code 'a'
             |> char_of_int,
             c2 )
         && c2 - c1 = 0
      || valid_pos (r2, c2 - c1 |> abs)
         && (r2 |> char_to_int) - (r1 |> char_to_int) = 0
  | _ -> false

let valid_rook_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      valid_pos
        ( ((r2 |> char_to_int) - (r1 |> char_to_int) |> abs) + Char.code 'a'
          |> char_of_int,
          c2 )
      && c2 - c1 = 0
      || valid_pos (r2, c2 - c1 |> abs)
         && (r2 |> char_to_int) - (r1 |> char_to_int) = 0
  | _ -> false

let valid_bishop_move pos1 pos2 =
  match (pos1, pos2) with
  | Some (r1, c1), Some (r2, c2) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = (c2 - c1 |> abs)
  | _ -> false

let is_in_vertical_path (pos1 : (char * int) option)
    (pos2 : (char * int) option) (pos3 : (char * int) option) =
  match (pos1, pos2, pos3) with
  | Some (r1, c1), Some (r2, c2), Some (r3, c3) ->
      (c1 = c2 && c2 = c3) && ((r1 >= r2 && r2 >= r3) || (r1 <= r2 && r2 <= r3))
  | _ -> false

let is_in_horizontal_path (pos1 : (char * int) option)
    (pos2 : (char * int) option) (pos3 : (char * int) option) =
  match (pos1, pos2, pos3) with
  | Some (r1, c1), Some (r2, c2), Some (r3, c3) ->
      r2 = r1 && r2 = r3 && ((c1 > c2 && c2 > c3) || (c1 < c2 && c2 < c3))
  | _ -> false

let is_in_diagonal_path (pos1 : (char * int) option)
    (pos2 : (char * int) option) (pos3 : (char * int) option) =
  match (pos1, pos2, pos3) with
  | Some (r1, c1), Some (r2, c2), Some (r3, c3) ->
      (r2 |> char_to_int) - (r1 |> char_to_int) |> abs = (c1 - c2 |> abs)
      && (r3 |> char_to_int) - (r2 |> char_to_int) |> abs = (c3 - c2 |> abs)
      && ((r1 < r2 && r2 < r3) || (r2 < r1 && r2 > r3))
      && ((c1 < c2 && c2 < c3) || (c2 < c1 && c2 > c3))
  | _ -> false
