(** Test Plan: At it's core, chess is a set of pieces with specified legal
    moves, and a game of chess is a matter of pieces making legal moves
    capturing each other until checkmate occurs. Therefore these two elements,
    the pieces and their legal moves, and "playing the game" itself are the
    divisions of our testing. Many of the interactions between pieces and game
    events can be tested visually by actually playing a game, such as capturing,
    castling, and checkmate. The validation of piece moves can be tested using
    OUnit passing in hypothetical positions and checking their legality. This
    means that we will test functions in module Piece and module Validate (alone
    with the basic initialization in modules State and Board) using OUnit and
    module State, module Command, and module Board (the more complex, mid-game
    functionality portions of State and Board) by playing the game run by main.
    The reason we can gurantee the certainty and correctness of these events and
    their interactions is because by using OUnit to test the move validation, we
    know that pieces cannot make illegal moves. By playing the game, we can
    visually see interactions working at hand, and if no pieces can make illegal
    move, then we can also gurantee that there are no illegal interactions
    between pieces and the game is in fact correct. *)

open OUnit2
open Game

let new_board =
  [
    [ "♖"; "♘"; "♗"; "♕"; "♔"; "♗"; "♘"; "♖" ];
    [ "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "♙"; "♙" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "♟"; "♟" ];
    [ "♜"; "♞"; "♝"; "♛"; "♚"; "♝"; "♞"; "♜" ];
  ]

let coordinate_converter_row ltr =
  match ltr with
  | 'a' -> "1"
  | 'b' -> "2"
  | 'c' -> "3"
  | 'd' -> "4"
  | 'e' -> "5"
  | 'f' -> "6"
  | 'g' -> "7"
  | 'h' -> "8"
  | _ -> failwith "Impossible"

let coordinate_converter_col nmbr =
  match nmbr with
  | '1' -> "h"
  | '2' -> "g"
  | '3' -> "f"
  | '4' -> "e"
  | '5' -> "d"
  | '6' -> "c"
  | '7' -> "b"
  | '8' -> "a"
  | _ -> failwith "Impossible"

let coordinate_converter (cmd : string) =
  coordinate_converter_col cmd.[1] ^ coordinate_converter_row cmd.[0]

let sample_test (name : string) test_output expected_output : test =
  name >:: fun _ -> assert_equal expected_output test_output

let b_king = Piece.string_to_piece "♔"
let w_queen = Piece.string_to_piece "♛"
let b_knight = Piece.string_to_piece "♘"
let w_rook = Piece.string_to_piece "♜"
let b_bishop = Piece.string_to_piece "♗"
let w_pawn = Piece.string_to_piece "♟"

let create_piece_tester (name : string) (piece : Piece.piece_type * Piece.color)
    (f : Piece.piece -> bool) (ch : char) (i : int) : test =
  name >:: fun _ ->
  assert_equal true
    (snd piece |> Piece.create_piece (fst piece) (Some (ch, i)) |> f)

let st = State.create_state (Board.init_board new_board)
let piece_loc_helper x = (Piece.get_piece_type x, Piece.get_color x)

let piece_at_loc c i =
  Some (c, i) |> Board.get_piece (State.get_board st) |> piece_loc_helper

let create_state_tester (name : string) (ch : char) (i : int)
    (piece : Piece.piece_type * Piece.color) : test =
  name >:: fun _ ->
  try assert_equal piece (piece_at_loc ch i)
  with Board.InvalidMove -> assert_equal true true

let create_piece_tests =
  [
    create_piece_tester "creating king piece" b_king Piece.is_king 'e' 8;
    create_piece_tester "creating queen piece" w_queen Piece.is_queen 'd' 1;
    create_piece_tester "creating knight piece" b_knight Piece.is_knight 'b' 8;
    create_piece_tester "creating rook piece" w_rook Piece.is_rook 'a' 1;
    create_piece_tester "creating bishop piece" b_bishop Piece.is_bishop 'c' 8;
    create_piece_tester "creating pawn piece" w_pawn Piece.is_pawn 'c' 7;
  ]

let create_state_tests =
  [
    create_state_tester "black king starting location" 'a' 5 b_king;
    create_state_tester "white queen starting location" 'h' 4 w_queen;
    create_state_tester "black knight starting location" 'a' 2 b_knight;
    create_state_tester "white rook starting location" 'h' 1 w_rook;
    create_state_tester "black bishop starting location" 'a' 3 b_bishop;
    create_state_tester "white pawn starting location" 'g' 7 w_pawn;
    create_state_tester "empty square initialized correctly" 'e' 4 w_pawn;
  ]

let valid_move_tests = []

let suite =
  "chess test suite"
  >::: List.flatten [ create_piece_tests; create_state_tests ]

let _ = run_test_tt_main suite
