
type board = Piece.piece list

let init_board board =
  let rec row x = function
    | [ [] ] -> []
    | h :: t ->
        let rec col y = function
          | [] -> []
          | h2 :: t2 ->
              if h2 = "-" then col (y + 1) t2
              else 
                Piece.create_piece (Piece.string_to_piece h2) (Some (char_of_int (x + 96), y)) (Piece.string_to_color "white") :: col (y + 1) t2
        in
        col 1 h @ row (x + 1) t
    | _ -> []
  in
  row 1 board

let board_to_list lst =
  let rec row x =
    if x = 9 then []
    else
      let rec col y =
        if y = 9 then []
        else if
          List.exists
            (fun a -> Piece.get_position a = Some (char_of_int (x + 96), y))
            lst
        then "P" :: col (y + 1) (** this only works for pawns*)
        else "-" :: col (y + 1)
      in
      col 1 :: row (x + 1)
  in
  row 1


let remove_piece (board: Piece.piece list) (piece: Piece.piece) = List.filter (fun x -> x <> piece) board
let add_piece (board : Piece.piece list) (piece:Piece.piece) = piece :: board

let get_piece (board : Piece.piece list) (pos: (char*int) option) = List.find (fun x-> Piece.get_position x = pos ) board

let move (board: Piece.piece list) (old_pos : (char*int) option) (new_pos : (char*int) option) = let (piece:Piece.piece) = get_piece board old_pos in let piece' = Piece.move_piece piece new_pos in let board' = remove_piece board piece in add_piece board' piece'
