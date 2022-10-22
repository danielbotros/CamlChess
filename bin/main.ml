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

let print_board lst =
  let rec helper c lst =
    match lst with
    | [] ->
        print_endline " ";
        print_endline "              1 2 3 4 5 6 7 8"
    | h :: t ->
        print_endline
          ("          " ^ String.make 1 c ^ "   " ^ String.concat " " h);
        helper (char_of_int (int_of_char c + 1)) t
  in
  helper 'a' lst

let rec play_game_helper st =
  print_endline "";
  print_board (State.board st);
  print_endline
    "\n\n\
     Enter 'move' followed by starting position and final position of the \
     desired move or 'quit' to exit:";
  match Command.parse (read_line ()) with
  | exception _ ->
      print_endline "";
      print_endline "This is not a valid move. Please try again: ";
      play_game_helper st
  | Move (x, y) -> (
      try
        play_game_helper
          (State.update_state st
             (Some (x.[0], int_of_char x.[1] - 48))
             (Some (y.[0], int_of_char y.[1] - 48)))
      with exn ->
        print_endline "";
        print_endline "This is not a valid move. Please try again: ";
        play_game_helper st)
  | Quit ->
      print_endline "\nGame over. Hope you enjoyed playing!\n";
      exit 0

(** [play_game new_board] starts the chess game. *)
let play_game new_board =
  play_game_helper (State.create_state (Board.init_board new_board))

let rec main_helper start =
  match start with
  | "yes" ->
      print_endline "\n\n♛  ♔  Welcome to your Game of Chess! ♔  ♛\n";
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
