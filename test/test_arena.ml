open Subarasushi.Arena
open Subarasushi.Cards
open Subarasushi.Player
open Utils

(** Trivial working player strategy. It just picks the first card in its hand
    for every decision and does not play special cards. *)
let first_pick_strategy =
  {
    choose_card = (fun _ player_status -> List.hd player_status.hand);
    play_chopsticks = (fun _ _ -> None);
    play_spoon = (fun _ _ -> None);
    choose_card_from_deck = (fun _ _ ~options -> List.hd options);
    choose_card_to_copy = (fun _ player -> List.hd player.table);
    choose_cards_to_flip = (fun _ _ -> []);
    choose_card_to_give = (fun _ _ ~options -> List.hd options);
  }

(** Creates a game with the [SushiGo] [menu] and [n] players. 
    They will use the [first_pick_strategy] to make their decisions.*)
let default_game_settings n =
  {
    players = List.init n (fun _ -> (first_pick_strategy, ""));
    menu = menu_of_default_menu SushiGo;
  }

let run_arena game_settings =
  let _ = Subarasushi.Arena.arena game_settings in
  ()

let are_players_sorted l =
  let rec is_sorted l =
    match l with
    | [] | [ _ ] -> true
    | h1 :: h2 :: t -> h1.score >= h2.score && is_sorted (h2 :: t)
  in
  is_sorted l

let get_winner_points = function Single x -> x | Draw l -> List.hd l

let winners_have_same_points_and_desserts = function
  | Single _ -> true
  | Draw l ->
      let first_player = List.hd l in
      List.for_all
        (fun x ->
          x.score = first_player.score
          && List.length x.desserts = List.length first_player.desserts)
        l

let win_data_is_accurate =
  let open QCheck in
  Test.make ~count:200 ~name:"Game ending data is accurate"
    (pair (int_range 2 8) arbitrary_menu)
    (fun (nb_players, menu) ->
      let game_settings =
        {
          players =
            List.init nb_players (fun i ->
                (first_pick_strategy, Printf.sprintf "Player %d" i));
          menu;
        }
      in
      let game_ending = arena game_settings in
      let winner_points = get_winner_points game_ending.winners in
      are_players_sorted game_ending.players
      && winner_points.score = (List.hd game_ending.players |> fun x -> x.score)
      && winners_have_same_points_and_desserts game_ending.winners)

let () =
  let open Alcotest in
  run "Arena"
    [
      ( "Game Initialization",
        [
          test_case "No players raises an exception" `Quick (fun () ->
              Alcotest.check_raises "No players raises an exception"
                (Invalid_argument "Not enough players") (fun () ->
                  run_arena (default_game_settings 0)));
          test_case "One player raises an exception" `Quick (fun () ->
              Alcotest.check_raises "One player raises an exception"
                (Invalid_argument "Not enough players") (fun () ->
                  run_arena (default_game_settings 1)));
          test_case "2 players does not raise an exception" `Quick (fun () ->
              Alcotest.check unit "2 players does not raise an exception" ()
                (run_arena (default_game_settings 2)));
          test_case "8 players does not raise an exception" `Quick (fun () ->
              Alcotest.check unit "8 players does not raise an exception" ()
                (run_arena (default_game_settings 8)));
          test_case "9 players raises an exception" `Quick (fun () ->
              Alcotest.check_raises "9 players raises an exception"
                (Invalid_argument "Too many players") (fun () ->
                  run_arena (default_game_settings 9)));
        ] );
      ( "Game ending accuracy",
        [ QCheck_alcotest.to_alcotest win_data_is_accurate ] );
      ( "Game ending draw",
        [
          test_case "Draw with 2 players" `Quick (fun () ->
              let players =
                List.init 2 (fun _ ->
                    let player = default_named_player "tst" in
                    { player with score = 10; desserts = [] })
              in
              let expected = Draw players in
              let actual = game_ending_of_player_list players in
              Alcotest.(check testable_win) "same" expected actual.winners);
          test_case "3 player draw in points but two have more desserts" `Quick
            (fun () ->
              let players =
                List.init 3 (fun i ->
                    let player = default_named_player "tst" in
                    {
                      player with
                      score = 10;
                      desserts = (if i = 2 then [] else [ Pudding ]);
                    })
              in
              let expected = Draw [ List.nth players 0; List.nth players 1 ] in
              let actual = game_ending_of_player_list players in
              Alcotest.(check testable_win) "same" expected actual.winners);
          test_case "3 player draw in points but one has more desserts" `Quick
            (fun () ->
              let players =
                List.init 3 (fun i ->
                    let player = default_named_player "tst" in
                    {
                      player with
                      score = 10;
                      desserts = (if i = 2 then [ Pudding ] else []);
                    })
              in
              let expected = Single (List.nth players 2) in
              let actual = game_ending_of_player_list players in
              Alcotest.(check testable_win) "same" expected actual.winners);
          test_case "1 winner in points but wih less desserts" `Quick (fun () ->
              let players =
                List.init 3 (fun i ->
                    let player = default_named_player "tst" in
                    {
                      player with
                      score = (if i = 2 then 100 else 10);
                      desserts = (if i = 2 then [] else [ Pudding ]);
                    })
              in
              let expected = Single (List.nth players 2) in
              let actual = game_ending_of_player_list players in
              Alcotest.(check testable_win) "same" expected actual.winners);
        ] );
    ]
