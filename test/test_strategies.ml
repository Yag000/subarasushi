open Subarasushi.Arena
open Subarasushi.Strategies
open Subarasushi.Cards.CardType
open Utils

let init_game player_number strategy menu =
  {
    players =
      List.init player_number (fun i -> (strategy, Printf.sprintf "player%d" i));
    menu;
  }

let menu_breaks_game menu player_number =
  let _, _, _, _, s1, s2, _ = menu in
  player_number > 6
  && (card_type_of_card (Special s1) = Menu
     || card_type_of_card (Special s1) = SpecialOrder
     || card_type_of_card (Special s2) = Menu
     || card_type_of_card (Special s2) = SpecialOrder)

let test_random_player_strategy =
  let open QCheck in
  Test.make ~count:10000
    ~name:
      "for all menus, the random player strategy should return a valid playing \
       style"
    (pair (int_range 2 8) arbitrary_menu)
    (fun (player_number, menu) ->
      let game = init_game player_number random_player menu in
      if player_number <= 1 || menu_breaks_game menu player_number then true
      else
        let _game_ending = arena game in
        true)

let () =
  Random.self_init ();
  let open Alcotest in
  run "Strategies"
    [
      ( "random_player",
        [ QCheck_alcotest.to_alcotest test_random_player_strategy ] );
    ]
