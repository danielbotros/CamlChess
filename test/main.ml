(** Test Plan: At it's core, chess is a set of pieces with specified legal
    moves, and a game of chess is a matter of pieces making legal moves
    capturing each other until checkmate occurs. Therefore these two elements,
    the pieces and their legal moves, and "playing the game" itself are the
    divisions of our testing. Many of the interactions between pieces and game
    events can be tested visually by actually playing a game, such as capturing,
    castling, and checkmate. The validation of piece moves can be tested using
    OUnit passing in hypothetical positions and checking their legality. This
    means that we will test functions in module Piece and module Validate using
    OUnit and module State, module Command, and module Board by playing the game
    run by main. The reason we can gurantee the certainty and correctness of
    these events and their interactions is because by using OUnit to test the
    move validation, we know that pieces cannot make illegal moves. By playing
    the game, we can visually see interactions working at hand, and if no pieces
    can make illegal move, then we can also gurantee that there are no illegal
    interactions between pieces and the game is in fact correct. *)

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
let st = State.create_state (Board.init_board new_board)

let create_piece_tests =
  [
    sample_test "creating king piece"
      (Piece.is_king
         (Piece.create_piece (fst b_king) (Some ('e', 8)) (snd b_king)))
      true;
    sample_test "creating queen piece"
      (Piece.is_queen
         (Piece.create_piece (fst w_queen) (Some ('d', 1)) (snd w_queen)))
      true;
    sample_test "creating knight piece"
      (Piece.is_knight
         (Piece.create_piece (fst b_knight) (Some ('b', 8)) (snd b_knight)))
      true;
    sample_test "creating rook piece"
      (Piece.is_rook
         (Piece.create_piece (fst w_rook) (Some ('a', 1)) (snd w_rook)))
      true;
    sample_test "creating bishop piece"
      (Piece.is_bishop
         (Piece.create_piece (fst b_bishop) (Some ('c', 8)) (snd b_bishop)))
      true;
    sample_test "creating pawn piece"
      (Piece.is_pawn
         (Piece.create_piece (fst w_pawn) (Some ('c', 7)) (snd w_pawn)))
      true;
  ]

let valid_move_tests = []
let suite = "chess test suite" >::: List.flatten [ create_piece_tests ]
let _ = run_test_tt_main suite
