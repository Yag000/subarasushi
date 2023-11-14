open Subarasushi

let () =
  (* Initialize the random number generator *)
  Random.self_init ();
  (* Get the game settings from the user *)
  let game_settings = Terminal_interaction.get_game_settings () in
  (* Play the game *)
  let game_ending = Arena.arena game_settings in
  Format.printf "Game ended:@.%a@." Display.pp_game_ending game_ending
