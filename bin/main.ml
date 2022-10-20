open Unit
open Game
open Command
open State

let new_board =
  [
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ];
    [ "p"; "p"; "p"; "p"; "p"; "p"; "p"; "p" ];
    [ "p"; "p"; "p"; "p"; "p"; "p"; "p"; "p" ];
  ]

let rec print_board = function
  | [] -> ()
  | h :: t ->
      print_endline (String.concat " " h);
      print_board t

let rec play_game_helper new_board st =
  print_board new_board;
  print_endline
    "Enter 'move' followed by starting position and final position of the \
     desired move or 'quit' to exit:";
  match Command.parse (read_line ()) with
  | exception _ ->
      print_endline "This is not a valid move. Please try again: ";
      play_game_helper new_board st
  | Go (x, y) ->
      print_endline "Valid move!";
      play_game_helper
        (Board.board_to_list State.board)
        (State.move (x.[0], int_of_char x.[1]) st (y.[0], int_of_char y.[1]))
  | Quit ->
      print_endline "Game over. Hope you enjoyed playing!";
      exit 0

(** [play_game new_board] starts the chess game. *)
let play_game new_board =
  print_endline "\n\nWelcome to your Game of Chess! ";
  play_game_helper new_board (State.create_state (Board.init_board new_board))

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to chess!\n";
  print_endline "Please enter anything to begin your game: \n";
  print_string "> ";
  match read_line () with
  | _ -> play_game new_board

(* Execute the game engine. *)
let () = main ()