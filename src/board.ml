type board = { pieces : Piece.piece list }

val rec init_board = function
| h :: t -> Piece.create_piece 
| _ -> failwith "empty list"
