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

let sample_test (name : string) test_output expected_output : test =
  name >:: fun _ -> assert_equal expected_output test_output

let king = Piece.string_to_piece "♔"
let pawn = Piece.string_to_piece "♟"

let piece_tests =
  [
    sample_test "creating king piece"
      (Piece.is_king (Piece.create_piece (fst king) (Some ('a', 1)) (snd king)))
      true;
    sample_test "creating pawn piece"
      (Piece.is_pawn (Piece.create_piece (fst pawn) (Some ('a', 1)) (snd pawn)))
      true;
  ]

let suite = "test suite for chess" >::: List.flatten [ piece_tests ]
let _ = run_test_tt_main suite
