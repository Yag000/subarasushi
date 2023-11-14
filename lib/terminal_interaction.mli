open Arena

val teletype_player : player_strategy
(** A player that gets its input from the terminal. *)

val get_game_settings : unit -> game_settings
(** [get_game_settings ()] prompts the user for the game settings. *)
