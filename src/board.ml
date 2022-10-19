type board = { pieces : Piece.piece list }

val rec init_board = function
| h :: t -> Piece.create_piece h 
| _ -> failwith "empty list"
