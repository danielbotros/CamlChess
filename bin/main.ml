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

let rec play_game_helper new_board =
  print_board new_board;
  print_endline
    "Enter 'go' followed by a valid exit to explore or enter 'quit' to exit:";
  match Command.parse (read_line ()) with
  | exception _ ->
      print_endline "This is not a valid move. Please try again: ";
      play_game_helper new_board
  | Go (x, y) -> print_endline ("(" ^ x ^ "," ^ y ^ ")")
  | Quit ->
      print_endline "Game over. Hope you enjoyed playing!";
      exit 0

(** [play_game new_board] starts the chess game. *)
let play_game new_board =
  print_endline "\n\nWelcome to your Game of Chess! ";
  (* play_game_helper new_board *)
  print_board new_board

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to chess!\n";
  print_endline "Please enter anything to begin your game: \n";
  print_string "> ";
  match read_line () with
  | _ -> print_board new_board

(* Execute the game engine. *)
let () = main ()