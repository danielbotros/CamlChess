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
