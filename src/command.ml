(* Note: You may introduce new code anywhere in this file. *)

type object_phrase = string * string

type command =
  | Move of object_phrase
  | Castle of object_phrase
  | Info of object_phrase
  | Quit

exception Empty
exception Malformed

let parse str =
  let no_spaces =
    List.filter (fun s -> s <> "") (String.split_on_char ' ' str)
  in
  match no_spaces with
  | [] -> raise Empty
  | h :: t -> begin
      match String.lowercase_ascii h with
      | "quit" -> if t <> [] then raise Malformed else Quit
      | ("move" | "castle") as cmd -> begin
          match t with
          | [] -> raise Malformed
          | [ _ ] -> raise Malformed
          | [ movef; movet ] ->
              let movefrom = String.lowercase_ascii movef in
              let moveto = String.lowercase_ascii movet in
              if
                (Char.code 'a' <= Char.code movefrom.[0]
                && Char.code movefrom.[0] <= Char.code 'h'
                && int_of_char movefrom.[1] - int_of_char '0' >= 1
                && int_of_char movefrom.[1] - int_of_char '0' <= 8
                && String.length movefrom = 2)
                && Char.code 'a' <= Char.code moveto.[0]
                && Char.code moveto.[0] <= Char.code 'h'
                && int_of_char moveto.[1] - int_of_char '0' >= 1
                && int_of_char moveto.[1] - int_of_char '0' <= 8
                && String.length moveto = 2
              then
                if cmd = "move" then Move (movefrom, moveto)
                else Castle (movefrom, moveto)
              else raise Malformed
          | _ -> raise Malformed
        end
      | "info" -> (
          match t with
          | [] -> raise Malformed
          | [ loc ] ->
              let piece_loc = String.lowercase_ascii loc in
              if
                Char.code 'a' <= Char.code piece_loc.[0]
                && Char.code piece_loc.[0] <= Char.code 'h'
                && int_of_char piece_loc.[1] - int_of_char '0' >= 1
                && int_of_char piece_loc.[1] - int_of_char '0' <= 8
                && String.length piece_loc = 2
              then Info (piece_loc, "")
              else raise Malformed
          | _ -> raise Malformed)
      | _ -> raise Malformed
    end
