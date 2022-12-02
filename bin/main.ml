open Unit
open Game
open Command
open State

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

let print_board board grave =
  let rec helper c bd gr =
    match (bd, gr) with
    | [], [ []; []; []; [] ] ->
        print_endline " ";
        print_endline "                    a b c d e f g h     "
    | [ h_b ], [ []; []; []; [] ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b);
        helper (char_of_int (int_of_char c - 1)) [] [ []; []; []; [] ]
    | [ h_b1; h_b2 ], [ []; []; []; [] ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b1);
        print_endline
          ("                "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "   " ^ String.concat " " h_b2);
        helper (char_of_int (int_of_char c - 2)) [] [ []; []; []; [] ]
    | [ h_b1; h_b2 ], [ []; []; b1; [] ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b1);
        print_endline
          ("                "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "   " ^ String.concat " " h_b2 ^ "      Black Graveyard: "
          ^ String.concat " " b1);
        helper (char_of_int (int_of_char c - 2)) [] [ []; []; []; [] ]
    | [ h_b1; h_b2 ], [ []; []; b1; b2 ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b1
         ^ "      Black Graveyard: " ^ String.concat " " b1);
        print_endline
          ("                "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "   " ^ String.concat " " h_b2 ^ "                       "
          ^ String.concat " " b2);
        helper (char_of_int (int_of_char c - 2)) [] [ []; []; []; [] ]
    | h_b :: t_b, [ []; []; b1; b2 ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b);
        helper (char_of_int (int_of_char c - 1)) t_b [ []; []; b1; b2 ]
    | h_b :: t_b, [ w1; []; b1; b2 ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b
         ^ "      White Graveyard: " ^ String.concat " " w1);
        helper (char_of_int (int_of_char c - 1)) t_b [ []; []; b1; b2 ]
    | h_b1 :: h_b2 :: t_b, [ w1; w2; b1; b2 ] ->
        print_endline
          ("                " ^ String.make 1 c ^ "   " ^ String.concat " " h_b1
         ^ "      White Graveyard: " ^ String.concat " " w1);
        print_endline
          ("                "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "   " ^ String.concat " " h_b2 ^ "                       "
          ^ String.concat " " w2);
        helper (char_of_int (int_of_char c - 2)) t_b [ []; []; b1; b2 ]
    (* | [ h_b ], [ []; b_g ] -> print_endline (" " ^ String.make 1 c ^ " " ^
       String.concat " " h_b ^ " Black Graveyard: " ^ String.concat " " b_g);
       helper (char_of_int (int_of_char c - 1)) [] [ []; b_g ] | h_b :: t_b, [
       []; b_g ] -> print_endline (" " ^ String.make 1 c ^ " " ^ String.concat "
       " h_b); helper (char_of_int (int_of_char c - 1)) t_b [ []; b_g ] | h_b ::
       t_b, [ w_g; b_g ] -> print_endline (" " ^ String.make 1 c ^ " " ^
       String.concat " " h_b ^ " White Graveyard: " ^ String.concat " " w_g);
       helper (char_of_int (int_of_char c - 1)) t_b [ []; b_g ] *)
    | _, _ -> failwith "Impossible"
  in
  helper '8' board grave

let rec grave_helper grave acc =
  match (grave, acc) with
  | [], acc -> acc
  | "♟" :: t, [ w1; w2; b1; b2 ] ->
      if List.length w1 < 8 then grave_helper t [ "♟" :: w1; w2; b1; b2 ]
      else grave_helper t [ w1; "♟" :: w2; b1; b2 ]
  | "♞" :: t, [ w1; w2; b1; b2 ] ->
      if List.length w1 < 8 then grave_helper t [ "♞" :: w1; w2; b1; b2 ]
      else grave_helper t [ w1; "♞" :: w2; b1; b2 ]
  | "♚" :: t, [ w1; w2; b1; b2 ] ->
      if List.length w1 < 8 then grave_helper t [ "♚" :: w1; w2; b1; b2 ]
      else grave_helper t [ w1; "♚" :: w2; b1; b2 ]
  | "♛" :: t, [ w1; w2; b1; b2 ] ->
      if List.length w1 < 8 then grave_helper t [ "♛" :: w1; w2; b1; b2 ]
      else grave_helper t [ w1; "♛" :: w2; b1; b2 ]
  | "♜" :: t, [ w1; w2; b1; b2 ] ->
      if List.length w1 < 8 then grave_helper t [ "♜" :: w1; w2; b1; b2 ]
      else grave_helper t [ w1; "♜" :: w2; b1; b2 ]
  | "♝" :: t, [ w1; w2; b1; b2 ] ->
      if List.length w1 < 8 then grave_helper t [ "♝" :: w1; w2; b1; b2 ]
      else grave_helper t [ w1; "♝" :: w2; b1; b2 ]
  | "♙" :: t, [ w1; w2; b1; b2 ] ->
      if List.length b1 < 8 then grave_helper t [ w1; w2; "♙" :: b1; b2 ]
      else grave_helper t [ w1; w2; b1; "♙" :: b2 ]
  | "♘" :: t, [ w1; w2; b1; b2 ] ->
      if List.length b1 < 8 then grave_helper t [ w1; w2; "♘" :: b1; b2 ]
      else grave_helper t [ w1; w2; b1; "♘" :: b2 ]
  | "♔" :: t, [ w1; w2; b1; b2 ] ->
      if List.length b1 < 8 then grave_helper t [ w1; w2; "♔" :: b1; b2 ]
      else grave_helper t [ w1; w2; b1; "♔" :: b2 ]
  | "♕" :: t, [ w1; w2; b1; b2 ] ->
      if List.length b1 < 8 then grave_helper t [ w1; w2; "♕" :: b1; b2 ]
      else grave_helper t [ w1; w2; b1; "♕" :: b2 ]
  | "♖" :: t, [ w1; w2; b1; b2 ] ->
      if List.length b1 < 8 then grave_helper t [ w1; w2; "♖" :: b1; b2 ]
      else grave_helper t [ w1; w2; b1; "♖" :: b2 ]
  | "♗" :: t, [ w1; w2; b1; b2 ] ->
      if List.length b1 < 8 then grave_helper t [ w1; w2; "♗" :: b1; b2 ]
      else grave_helper t [ w1; w2; b1; "♗" :: b2 ]
  | _ -> failwith "Invalid call to grave_helper"

let rec play_game_helper st =
  print_endline "";
  print_board (State.board st)
    (grave_helper (State.graveyard st) [ []; []; []; [] ]);
  print_endline
    "\n\n\
     Enter 'move' followed by starting position and final position\n\
     of the desired move (for example: move g2 e2) or 'quit' to exit:";
  match Command.parse (read_line ()) with
  | exception _ ->
      print_endline "";
      print_endline "     This is not a valid move. Please try again: ";
      play_game_helper st
  | Move (x, y) -> (
      let x' = coordinate_converter x in
      let y' = coordinate_converter y in
      try
        play_game_helper
          (State.update_state false st
             (Some (x'.[0], int_of_char x'.[1] - 48))
             (Some (y'.[0], int_of_char y'.[1] - 48)))
      with exn ->
        print_endline "";
        print_endline "           This is not a valid move. Please try again: ";
        play_game_helper st)
  | Castle (x, y) -> (
      let x' = coordinate_converter x in
      let y' = coordinate_converter y in
      try
        play_game_helper
          (State.update_state true st
             (Some (x'.[0], int_of_char x'.[1] - 48))
             (Some (y'.[0], int_of_char y'.[1] - 48)))
      with exn ->
        print_endline "";
        print_endline "This is not a valid castle. Please try again: ";
        play_game_helper st)
  | Quit ->
      print_endline "\n           Game over. Hope you enjoyed playing!\n";
      exit 0

(** [play_game new_board] starts the chess game. *)
let play_game new_board =
  play_game_helper (State.create_state (Board.init_board new_board))

let rec main_helper start =
  match start with
  | "yes" ->
      print_endline "\n\n     ♛  ♔  Welcome to your Game of Chess! ♔  ♛\n";
      play_game new_board
  | "no" ->
      print_endline
        "\n\
        \ (>'-'>)  Every new beginning comes from some other beginning's end \n";
      exit 0
  | _ ->
      print_endline "\nWait what?\n";
      main_helper (read_line ())

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\n         Welcome to chess!\n";
  print_endline "Do you want to start a game? (yes/no) \n";
  print_string "> ";
  main_helper (read_line ())

(* Execute the game engine. *)
let () = main ()
