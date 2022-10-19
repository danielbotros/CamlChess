open Unit

(** [play_game] starts the chess game. *)
(* let play_game = raise (Failure "Unimplemented: main.play_game") *)

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

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\nWelcome to chess!\n";
  print_endline "Please enter anything to begin your game: \n";
  print_string "> ";
  match read_line () with
  | _ -> print_board new_board

(* Execute the game engine. *)
let () = main ()