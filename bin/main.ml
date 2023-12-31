open Unit
open Game
open Command
open State

type gui = string list list

let (new_board : gui) =
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
(* Stalemate board: [ [ "-"; "-"; "♛"; "-"; "-"; "♗"; "♘"; "♖" ]; [ "-"; "-";
   "-"; "-"; "♙"; "-"; "♙"; "♕" ]; [ "-"; "-"; "-"; "-"; "-"; "♙"; "♔"; "♖" ]; [
   "-"; "-"; "-"; "-"; "-"; "-"; "-"; "♙" ]; [ "-"; "-"; "-"; "-"; "-"; "-";
   "-"; "♟" ]; [ "-"; "-"; "-"; "-"; "♟"; "-"; "-"; "-" ]; [ "♟"; "♟"; "♟"; "♟";
   "-"; "♟"; "♟"; "-" ]; [ "♜"; "♞"; "♝"; "-"; "♚"; "♝"; "♞"; "♜" ]; ]

   en passant: [ [ "♖"; "♘"; "♗"; "♕"; "♔"; "♗"; "♘"; "♖" ]; [ "♙"; "-"; "♙";
   "♙"; "♙"; "♙"; "♙"; "-" ]; [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "♙" ]; [ "♟";
   "♙"; "-"; "-"; "-"; "-"; "-"; "-" ]; [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-"
   ]; [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ]; [ "-"; "♟"; "♟"; "♟"; "♟";
   "♟"; "♟"; "♟" ]; [ "♜"; "♞"; "♝"; "♛"; "♚"; "♝"; "♞"; "♜" ]; ]

   chess promotion: [ [ "♖"; "♘"; "♗"; "♕"; "♔"; "♗"; "♘"; "♖" ]; [ "♟"; "♙";
   "♙"; "♙"; "♙"; "♙"; "♙"; "-" ]; [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ]; [
   "-"; "-"; "-"; "-"; "-"; "-"; "-"; "-" ]; [ "-"; "-"; "-"; "-"; "-"; "-";
   "-"; "-" ]; [ "-"; "-"; "-"; "-"; "-"; "-"; "-"; "♙" ]; [ "♟"; "-"; "♟"; "♟";
   "♟"; "♟"; "♟"; "♟" ]; [ "♜"; "♞"; "♝"; "♛"; "♚"; "♝"; "♞"; "♜" ]; ]*)

let create_gui () = new_board

let coordinate_converter_row ltr rev =
  match ltr with
  | 'a' -> if rev then "8" else "1"
  | 'b' -> if rev then "7" else "2"
  | 'c' -> if rev then "6" else "3"
  | 'd' -> if rev then "5" else "4"
  | 'e' -> if rev then "4" else "5"
  | 'f' -> if rev then "3" else "6"
  | 'g' -> if rev then "2" else "7"
  | 'h' -> if rev then "1" else "8"
  | _ -> failwith "Impossible"

let coordinate_converter_col nmbr rev =
  match nmbr with
  | '1' -> if rev then "a" else "h"
  | '2' -> if rev then "b" else "g"
  | '3' -> if rev then "c" else "f"
  | '4' -> if rev then "d" else "e"
  | '5' -> if rev then "e" else "d"
  | '6' -> if rev then "f" else "c"
  | '7' -> if rev then "g" else "b"
  | '8' -> if rev then "h" else "a"
  | _ -> failwith "Impossible"

let coordinate_converter (cmd : string) (rev : bool) =
  if rev then
    (* print_endline ("Converting to good:" ^ cmd ^ ", " ^ string_of_bool
       rev); *)
    coordinate_converter_col cmd.[1] true
    ^ coordinate_converter_row cmd.[0] true
  else
    (* print_endline ("Converting to bad:" ^ cmd ^ ", " ^ string_of_bool
       rev); *)
    coordinate_converter_col cmd.[1] false
    ^ coordinate_converter_row cmd.[0] false

let print_board (board : gui) (grave : gui) (moves1, moves2, moves3) =
  let rec helper c bd gr =
    match (bd, gr) with
    | [], [ []; []; []; [] ] ->
        print_endline "";
        print_endline "                 a   b   c   d   e   f   g   h "
    | [ h_b ], [ []; []; []; [] ] ->
        print_endline
          ("          " ^ String.make 1 c ^ "    | " ^ String.concat "   " h_b
         ^ " |");
        print_endline "";
        helper (char_of_int (int_of_char c - 1)) [] [ []; []; []; [] ]
    | [ h_b1; h_b2 ], [ []; []; []; [] ] ->
        print_endline
          ("          " ^ String.make 1 c ^ "    | " ^ String.concat "   " h_b1
         ^ " |");
        print_endline "";
        print_endline
          ("          "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "    | " ^ String.concat "   " h_b2 ^ " |");
        print_endline "                 ―   ―   ―   ―   ―   ―   ―   ―";
        helper (char_of_int (int_of_char c - 2)) [] [ []; []; []; [] ]
    | [ h_b1; h_b2 ], [ []; []; b1; [] ] ->
        print_endline
          ("          " ^ String.make 1 c ^ "    | " ^ String.concat "   " h_b1
         ^ " |");
        print_endline "";
        print_endline
          ("          "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "    | " ^ String.concat "   " h_b2 ^ " |    Black Graveyard: "
          ^ String.concat " | " b1);
        print_endline "                 ―   ―   ―   ―   ―   ―   ―   ―";
        helper (char_of_int (int_of_char c - 2)) [] [ []; []; []; [] ]
    | [ h_b1; h_b2 ], [ []; []; b1; b2 ] ->
        print_endline
          ("          " ^ String.make 1 c ^ "    | " ^ String.concat "   " h_b1);
        print_endline "";
        print_endline
          ("          "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "    | " ^ String.concat "   " h_b2 ^ " |    Black Graveyard: "
          ^ String.concat " | " b1);
        print_endline
          ("                 ―   ―   ―   ―   ―   ―   ―   \
            ―                       " ^ String.concat " | " b2);
        helper (char_of_int (int_of_char c - 2)) [] [ []; []; []; [] ]
    | h_b1 :: h_b2 :: t_b, [ []; []; b1; b2 ] ->
        if c = '8' then (
          print_endline "                 ―   ―   ―   ―   ―   ―   ―   ―";
          print_endline
            ("          " ^ String.make 1 c ^ "    | "
           ^ String.concat "   " h_b1 ^ " |");
          print_endline "";
          helper
            (char_of_int (int_of_char c - 1))
            (h_b2 :: t_b) [ []; []; b1; b2 ])
        else if c = '5' then (
          print_endline
            ("          " ^ String.make 1 c ^ "    | "
           ^ String.concat "   " h_b1 ^ " |" ^ "     " ^ moves1);
          print_endline
            ("                                                                 "
           ^ moves2);
          print_endline
            ("          "
            ^ String.make 1 (char_of_int (int_of_char c - 1))
            ^ "    | " ^ String.concat "   " h_b2 ^ " |" ^ "                 "
            ^ moves3);
          print_endline "";
          helper (char_of_int (int_of_char c - 2)) t_b [ []; []; b1; b2 ])
        else (
          print_endline
            ("          " ^ String.make 1 c ^ "    | "
           ^ String.concat "   " h_b1 ^ " |");
          print_endline "";
          helper
            (char_of_int (int_of_char c - 1))
            (h_b2 :: t_b) [ []; []; b1; b2 ])
    | h_b :: t_b, [ w1; []; b1; b2 ] ->
        print_endline "                 ―   ―   ―   ―   ―   ―   ―   ―";
        print_endline
          ("          " ^ String.make 1 c ^ "    | " ^ String.concat "   " h_b
         ^ " |    White Graveyard: " ^ String.concat " | " w1);
        print_endline "";
        helper (char_of_int (int_of_char c - 1)) t_b [ []; []; b1; b2 ]
    | h_b1 :: h_b2 :: t_b, [ w1; w2; b1; b2 ] ->
        print_endline "                 ―   ―   ―   ―   ―   ―   ―   ―";
        print_endline
          ("          " ^ String.make 1 c ^ "    | " ^ String.concat "   " h_b1
         ^ " |    White Graveyard: " ^ String.concat " | " w1);
        print_endline
          ("                                                                     "
         ^ String.concat " | " w2);
        print_endline
          ("          "
          ^ String.make 1 (char_of_int (int_of_char c - 1))
          ^ "    | " ^ String.concat "   " h_b2 ^ " |                     ");
        print_endline "";
        helper (char_of_int (int_of_char c - 2)) t_b [ []; []; b1; b2 ]
    | _, _ -> failwith "Impossible"
  in
  print_endline "";
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

let past_moves st =
  let p, coord = State.get_past_moves st in
  List.map2
    (fun sym ((c1, i1), (c2, i2)) ->
      sym ^ "  "
      ^ coordinate_converter (String.make 1 c1 ^ string_of_int i1) true
      ^ " -> "
      ^ coordinate_converter (String.make 1 c2 ^ string_of_int i2) true)
    p coord

let past_helper st =
  match past_moves st with
  | h1 :: h2 :: h3 :: h4 :: h5 :: h6 :: t ->
      if State.get_turn st mod 2 = 0 then
        ("Past Moves: " ^ h5 ^ " | " ^ h4, h3 ^ " | " ^ h2, h1)
      else ("Past Moves: " ^ h6 ^ " | " ^ h5, h4 ^ " | " ^ h3, h2 ^ " | " ^ h1)
  | [ h1; h2; h3; h4; h5 ] ->
      ("Past Moves: " ^ h5 ^ " | " ^ h4, h3 ^ " | " ^ h2, h1)
  | [ h1; h2; h3; h4 ] -> ("Past Moves: " ^ h4 ^ " | " ^ h3, h2 ^ " | " ^ h1, "")
  | [ h1; h2; h3 ] -> ("Past Moves: " ^ h3 ^ " | " ^ h2, h1, "")
  | [ h1; h2 ] -> ("Past Moves: " ^ h2 ^ " | " ^ h1, "", "")
  | [ h ] -> ("Past Moves: " ^ h, "", "")
  | [] -> ("", "", "")

let checkmate_message st =
  if State.get_turn st mod 4 = 1 then
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\n\
      \        No moves can save the Black King now. Checkmate!\n\
      \                      Thanks for playing! \n\n"
  else if State.get_turn st mod 4 = 2 then
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\n\
      \        Nowhere to go for the White King. Checkmate!\n\
      \                      Thanks for playing! \n\n"
  else if State.get_turn st mod 4 = 3 then
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\n\
      \         The Black King has fallen. Checkmate!\n\
      \                      Thanks for playing! \n\n"
  else if State.get_turn st mod 4 = 0 then
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      "\n\
      \   This is the end of the road for the White King. Checkmate!\n\
      \                      Thanks for playing! \n\n"

let rec play_game_helper st info ai =
  print_endline "";
  if info then
    let past_moves = past_helper st in
    print_board (State.board st)
      (grave_helper (State.graveyard st) [ []; []; []; [] ])
      past_moves
  else ();
  if State.get_turn st mod 2 = 0 && ai then (
    print_endline
      "\n\n\
      \                     〖  Turn: Black  〗\n\
      \ Enter your desired move (for example: move e7 e6) or 'quit' to exit:";
    ANSITerminal.print_string [ ANSITerminal.yellow ]
      " \n                       AI is thinking...";
    print_endline "")
  else if State.get_turn st mod 2 = 0 then
    print_endline
      "\n\n\
      \                     〖  Turn: Black  〗\n\
      \ Enter your desired move (for example: move e7 e6) or 'quit' to exit:"
  else
    print_endline
      "\n\n\
      \                     【  Turn: White  】 \n\
      \ Enter your desired move (for example: move e2 e3) or 'quit' to exit:";
  if State.get_turn st mod 2 = 0 && ai then
    let state = Ai.optimal_state st in
    let () = print_string "> Oh, I know!" in
    play_game_helper state true ai
  else print_string "> ";
  match Command.parse (read_line ()) with
  | exception _ ->
      print_endline "";
      ANSITerminal.print_string [ ANSITerminal.red ]
        "      This is not a valid move command. Please try again! ";
      play_game_helper st true ai
  | Move (x, y) -> (
      let x' = coordinate_converter x false in
      let y' = coordinate_converter y false in
      (* print_endline (coordinate_converter x' true ^ coordinate_converter y'
         true); *)
      try
        play_game_helper
          (State.update_state false false st
             (Some (x'.[0], int_of_char x'.[1] - 48))
             (Some (y'.[0], int_of_char y'.[1] - 48)))
          true ai
      with
      | CheckMate -> checkmate_message st
      | StaleMate -> print_endline "Stalemate! No more valid moves."
      | Check ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "          Invalid move. This puts your king in check! ";
          play_game_helper st true ai
      | exn ->
          print_endline "";
          if State.get_turn st mod 2 = 1 then
            ANSITerminal.print_string [ ANSITerminal.red ]
              "   Attempted move is not a valid white move. Please try again! "
          else
            ANSITerminal.print_string [ ANSITerminal.red ]
              "   Attempted move is not a valid black move. Please try again! ";
          play_game_helper st true ai)
  | Castle (x, y) -> (
      let x' = coordinate_converter x false in
      let y' = coordinate_converter y false in
      try
        play_game_helper
          (State.update_state true false st
             (Some (x'.[0], int_of_char x'.[1] - 48))
             (Some (y'.[0], int_of_char y'.[1] - 48)))
          true ai
      with
      | CheckMate -> checkmate_message st
      | StaleMate -> print_endline "Stalemate! No more valid moves."
      | Check ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "          Invalid move. This puts your king in check! ";
          play_game_helper st true ai
      | exn ->
          print_endline "";
          print_endline
            "         This is not a valid castle. Please try again: ";
          play_game_helper st true ai)
  | Info (x, _) ->
      let x' = coordinate_converter x false in
      let move_list =
        State.possible_moves st (x'.[0], int_of_char x'.[1] - 48)
      in
      let board = State.board_info st move_list in
      let past_moves = past_helper st in
      let _ =
        print_board board
          (grave_helper (State.graveyard st) [ []; []; []; [] ])
          past_moves
      in
      play_game_helper st false ai
  | Quit ->
      print_endline "\n           Game over. Hope you enjoyed playing!\n";
      exit 0

(** [play_game new_board] starts the chess game. *)
let rec play_game gui ai =
  play_game_helper (State.create_state (Board.init_board gui)) true ai

let rec main_helper start n =
  match start with
  | "yes" ->
      let rec ai_helper x =
        print_endline
          "\n Would you like to play with AI? (Enter \"yes\" or \"no\")";
        print_endline "";
        print_string "> ";
        match read_line () with
        | "yes" | "AI" ->
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              "\n\n         ♛  ♔  Welcome to your Game of Chess! ♔  ♛\n";
            play_game (create_gui ()) true
        | "no" ->
            ANSITerminal.print_string [ ANSITerminal.yellow ]
              "\n\n         ♛  ♔  Welcome to your Game of Chess! ♔  ♛\n";
            play_game (create_gui ()) false
        | _ ->
            print_endline "\n             I don't understand.";
            ai_helper 1
      in
      ai_helper 1
  | "no" ->
      begin
        match n mod 3 with
        | 0 ->
            print_endline "\n             No chess today? That's alright! ";
            print_endline "   (ﾉ^_^)ﾉ   Every new beginning comes from";
            print_endline "               some other beginning's end \n"
        | 1 ->
            print_endline
              "\n     I hope you chase your dreams and live    ᕦ(ò_óˇ)ᕤ";
            print_endline "         the life you've always wanted!"
        | _ ->
            print_endline "\n ୧(^˽^)୨  Chess will always be here";
            print_endline "          when you are ready to play "
      end;

      exit 0
  | _ ->
      print_endline "\n             I don't understand.";
      print_endline "        Do you want to start a game? ";
      ANSITerminal.print_string [ ANSITerminal.yellow ]
        "        (Hint: Enter \"yes\" or  \"no\")\n";
      print_endline "";
      print_string "> ";
      main_helper (read_line ()) (n + 1)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_endline "\n\n                 Welcome to chess!\n";
  print_endline "         Do you want to start a game? (yes/no) \n";
  print_string "> ";
  main_helper (read_line ()) 0

(* Execute the game engine. *)
let () = main ()
