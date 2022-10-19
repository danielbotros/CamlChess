type color =
  | White
  | Black

type position = int * int

type square = position * color
(** The type representing a particular square *)

type direction =
  | North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest
  | K

type piece =
  | Pawn of square
  | Knight of square
  | King of square
  | Queen of square
  | Rook of square
  | Bishop of square

type board = (piece option * square) list

type t = {
  b : board;
  turn : color;
  active_pieces : piece list;
  captured_pieces : piece list;
}
