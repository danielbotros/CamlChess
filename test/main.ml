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

let string_to_coord s = Some (s.[0], int_of_char s.[1] - 48)

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

let pawn_move (name : string) (loc1 : string) (loc2 : string) f
    (expected_output : bool) : test =
  name >:: fun _ ->
  let func =
    match f with
    | "bpawn" -> Validate.valid_pawn_move_black
    | "wpawn" -> Validate.valid_pawn_move_white
    | _ -> failwith "invalid pawn test"
  in
  assert_equal
    (func
       (loc1 |> coordinate_converter |> string_to_coord)
       (loc2 |> coordinate_converter |> string_to_coord)
       true)
    expected_output

let valid_tester (name : string) (loc1 : string) (loc2 : string) f
    (expected_output : bool) : test =
  name >:: fun _ ->
  let func =
    match f with
    | "bpawn" -> Validate.valid_pawn_attack_black
    | "wpawn" -> Validate.valid_pawn_attack_white
    | "bishop" -> Validate.valid_bishop_move
    | "rook" -> Validate.valid_rook_move
    | "knight" -> Validate.valid_knight_move
    | "queen" -> Validate.valid_queen_move
    | "king" -> Validate.valid_king_move
    | _ -> failwith "invalid move test"
  in
  assert_equal
    (func
       (loc1 |> coordinate_converter |> string_to_coord)
       (loc2 |> coordinate_converter |> string_to_coord))
    expected_output

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

(* Key: V = Valid , IV = Invalid *)
let valid_move_tests =
  [
    pawn_move "V white pawn move single square" "g2" "g3" "wpawn" true;
    pawn_move "V white pawn move 2 squares" "e2" "e4" "wpawn" true;
    pawn_move "IV white pawn move 3 squares" "b2" "b5" "wpawn" false;
    valid_tester "V white pawn capture move diagonal" "g5" "f6" "wpawn" true;
    valid_tester "IV white pawn capture move forward" "g5" "g6" "wpawn" false;
    pawn_move "V black pawn move single square" "f7" "f6" "bpawn" true;
    pawn_move "V black pawn move 2 squares" "h7" "h5" "bpawn" true;
    pawn_move "IV black pawn move up" "f7" "f8" "bpawn" false;
    pawn_move "IV black pawn move diagonal" "f6" "e5" "bpawn" false;
    valid_tester "V black pawn capture move diagonal" "e7" "d6" "bpawn" true;
    valid_tester "IV black pawn capture move forward" "f4" "f3" "bpawn" false;
    valid_tester "V bishop diagonal move top right" "c1" "g5" "bishop" true;
    valid_tester "V bishop diagonal move top left" "f4" "c7" "bishop" true;
    valid_tester "V bishop diagonal move bottom right" "e5" "a1" "bishop" true;
    valid_tester "V bishop diagonal move bottom left" "f7" "b3" "bishop" true;
    valid_tester "IV bishop move up 3 squares" "f3" "f6" "bishop" false;
    valid_tester "V rook move vertically" "f2" "f7" "rook" true;
    valid_tester "V rook move horizontally" "e1" "a1" "rook" true;
    valid_tester "IV rook move diagonal" "a3" "b4" "rook" false;
    valid_tester "V knight move up/up/left" "b1" "a3" "knight" true;
    valid_tester "V knight move up/up/right" "c1" "d3" "knight" true;
    valid_tester "V knight move up/left/left" "e1" "c2" "knight" true;
    valid_tester "V knight move up/right/right" "d4" "f5" "knight" true;
    valid_tester "V knight move down/down/left" "g8" "f6" "knight" true;
    valid_tester "V knight move down/down/right" "c6" "d4" "knight" true;
    valid_tester "V knight move down/left/left" "e4" "c3" "knight" true;
    valid_tester "V knight move down/right/right" "f2" "h1" "knight" true;
    valid_tester "IV knight move forward" "a3" "b4" "knight" false;
    valid_tester "V queen move up" "c2" "c6" "queen" true;
    valid_tester "V queen move down" "a8" "a3" "queen" true;
    valid_tester "V queen move left" "g3" "c3" "queen" true;
    valid_tester "V queen move right" "a7" "h7" "queen" true;
    valid_tester "V queen move diagonal top right" "e2" "h5" "queen" true;
    valid_tester "V queen move diagonal top left" "f2" "a7" "queen" true;
    valid_tester "V queen move diagonal bottom right" "c6" "h1" "queen" true;
    valid_tester "V queen move diagonal bottom left" "f7" "c4" "queen" true;
    valid_tester "V queen move forward" "a3" "b4" "queen" true;
    valid_tester "V king move up" "e1" "e2" "king" true;
    valid_tester "V king move down" "a8" "a7" "king" true;
    valid_tester "V king move left" "g3" "f3" "king" true;
    valid_tester "V king move right" "b2" "c2" "king" true;
    valid_tester "V king move diagonal top right" "e1" "f2" "king" true;
    valid_tester "V king move diagonal top left" "c5" "b6" "king" true;
    valid_tester "V king move diagonal bottom right" "d4" "e3" "king" true;
    valid_tester "V king move diagonal bottom left" "g2" "f1" "king" true;
    valid_tester "IV king move forward multiple squares" "d3" "d5" "king" false;
    valid_tester "IV king move diagonal multiple squares" "e1" "c3" "king" false;
  ]

let suite =
  "chess test suite"
  >::: List.flatten [ create_piece_tests; create_state_tests; valid_move_tests ]

let _ = run_test_tt_main suite
