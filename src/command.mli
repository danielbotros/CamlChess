(** Parsing of player commands. *)

type object_phrase = string * string
(** The type [object_phrase] represents the object phrase that can be part of a
    player command. Each element of the list represents a word of the object
    phrase, where a "word" is defined as a consecutive sequence of non-space
    characters. Thus, no element of the list should contain any leading,
    internal, or trailing spaces. The list is in the same order as the words in
    the original player command. For example:

    - If the player command is ["move e2 e4"], then the object phrase is
      [\["e2"; "e4"\]].

    - If the player command is ["move e2     e4"], then the object phrase is
      again [\["e2"; "e4"\]]. *)

(** The type [command] represents a player command that is decomposed into a
    verb and possibly an object phrase. Invariant: the [object_phrase] carried
    by [Move] must not be empty. *)
type command =
  | Move of object_phrase
  | Castle of object_phrase
  | Info of object_phrase
  | Quit

exception Empty
(** Raised when an empty command is parsed. *)

exception Malformed
(** Raised when a malformed command is parsed. *)

val parse : string -> command
(** [parse str] parses a player's input into a [command], as follows. The first
    word (i.e., consecutive sequence of non-space characters) of [str] becomes
    the verb. The rest of the words, if any, become the object phrase. Examples:

    - [parse "    move   e2   e4   "] is [Go \["e2"; "e4"\]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines, etc.).

    Raises: [Empty] if [str] is the empty string or contains only spaces.

    Raises: [Malformed] if the command is malformed. A command is malformed if
    the verb is neither "quit", "castle", "info" nor "move", or if the verb is
    "quit" and there is a non-empty object phrase, or if the verb is "move" and
    there is an empty object phrase. A command is also malformed if it's
    object_phrase is not a valid position on the board.*)
