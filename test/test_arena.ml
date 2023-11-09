open Subarasushi.Arena
open Subarasushi.Cards

(** Trivial working player strategy. It just picks the first card in its hand
    for every decision and does not play special cards. *)
let first_pick_strategy =
  {
    choose_card = (fun _ player_status -> List.hd player_status.hand);
    play_chopsticks = (fun _ _ -> None);
    play_spoon = (fun _ _ -> None);
    choose_card_from_deck = (fun _ player_status -> List.hd player_status.hand);
    choose_card_to_copy = (fun _ player_status -> List.hd player_status.hand);
    choose_cards_to_flip = (fun _ _ -> []);
    choose_card_to_give = (fun _ player_status -> List.hd player_status.hand);
  }

(** Creates a game with the [SushiGo] [menu] and [n] players. 
    They will use the [first_pick_strategy] to make their decisions.*)
let default_game_settings n =
  {
    players = List.init n (fun _ -> (first_pick_strategy, ""));
    menu = menu_of_default_menu SushiGo;
  }

let () =
  let open Alcotest in
  run "Arena"
    [
      ( "Game Initialization",
        [
          test_case "No players raises an exception" `Quick (fun () ->
              Alcotest.check_raises "No players raises an exception"
                (Invalid_argument "Not enough players") (fun () ->
                  arena (default_game_settings 0)));
          test_case "One player raises an exception" `Quick (fun () ->
              Alcotest.check_raises "One player raises an exception"
                (Invalid_argument "Not enough players") (fun () ->
                  arena (default_game_settings 1)));
          test_case "2 players does not raise an exception" `Quick (fun () ->
              Alcotest.check unit "2 players does not raise an exception" ()
                (arena (default_game_settings 2)));
          test_case "8 players does not raise an exception" `Quick (fun () ->
              Alcotest.check unit "8 players does not raise an exception" ()
                (arena (default_game_settings 8)));
          test_case "9 players raises an exception" `Quick (fun () ->
              Alcotest.check_raises "9 players raises an exception"
                (Invalid_argument "Too many players") (fun () ->
                  arena (default_game_settings 9)));
        ] );
    ]
