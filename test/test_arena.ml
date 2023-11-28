open Subarasushi.Arena
open Subarasushi.Cards
open Subarasushi.Player
open Subarasushi.Utils
open Utils

exception MockTestException of string

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

let mock_test_non_empty_hand_when_copying =
  {
    first_pick_strategy with
    choose_card_to_copy =
      (fun _ player ->
        if player.table = [] then raise (MockTestException "Empty table")
        else List.hd player.table);
  }

let mock_test_non_empty_options =
  {
    first_pick_strategy with
    choose_card_from_deck =
      (fun _ _ ~options ->
        if options = [] then raise (MockTestException "Empty deck")
        else List.hd options);
    choose_card_to_give =
      (fun _ _ ~options ->
        if options = [] then raise (MockTestException "Empty card to give")
        else List.hd options);
  }

(** Forgive me Ocaml gods for what I have done, the test is necessary, but its 
    implementation is not pretty. I am deeply sorry. *)
let ref_total_turns = ref 0

let mock_test_number_turns =
  {
    first_pick_strategy with
    choose_card =
      (fun game_status player_status ->
        ref_total_turns := !ref_total_turns + 1;
        first_pick_strategy.choose_card game_status player_status);
  }

let possible_cards =
  [
    SushiRoll (Maki 1);
    SushiRoll (Uramaki 3);
    SushiRoll Temaki;
    Nigiri Squid;
    Nigiri Salmon;
    Nigiri Egg;
    Appetizer Tempura;
    Appetizer Sashimi;
    Appetizer Dumpling;
    Appetizer Edamame;
    Appetizer Tofu;
    Appetizer (Onigiri Circle);
    Appetizer Dumpling;
    Special (Wasabi None);
    Special (Spoon 4);
    Special (Chopsticks 1);
    Special (Menu 9);
    Special (TakeOutBox 11);
    Special SoySauce;
    Special SpecialOrder;
    Special Tea;
    Dessert Pudding;
    Dessert (Fruit [ Orange; Orange ]);
    Dessert MatchaIceCream;
  ]

let mock_always_plays_impossible_cards =
  {
    first_pick_strategy with
    choose_card =
      (fun _ player_status ->
        ref_total_turns := !ref_total_turns + 1;

        List.filter
          (fun card -> not (List.mem card player_status.hand))
          possible_cards
        |> List.hd);
  }

(** Creates a player strategy that always picks the same card if it is in its
    hand, otherwise it picks the first card in its hand. *)
let mock_prioritize_one_card card =
  {
    first_pick_strategy with
    choose_card =
      (fun _ player_status ->
        if List.mem card player_status.hand then card
        else List.hd player_status.hand);
  }

(** Creates a player strategy that always picks the same card type if it is in its
    hand, otherwise it picks the first card in its hand. *)
let mock_prioritize_one_card_type card_type =
  {
    first_pick_strategy with
    choose_card =
      (fun game_status player_status ->
        let filtered_cards =
          List.filter
            (fun card -> CardType.card_type_of_card card = card_type)
            player_status.hand
        in
        if filtered_cards = [] then
          first_pick_strategy.choose_card game_status player_status
        else List.hd filtered_cards);
  }

let mock_assert_chopstics_call =
  let chop_strat = mock_prioritize_one_card_type CardType.Chopsticks in
  {
    chop_strat with
    play_chopsticks = (fun _ _ -> raise (MockTestException "Chopsticks call"));
  }

let mock_assert_spoon_call =
  let spoon_strat = mock_prioritize_one_card (Special (Spoon 4)) in
  {
    spoon_strat with
    play_spoon = (fun _ _ -> raise (MockTestException "Spoon call"));
  }

(** Creates a game with the [SushiGo] [menu] and [n] players. 
    They will use the [first_pick_strategy] to make their decisions.*)
let default_game_settings n =
  {
    players = List.init n (fun _ -> (first_pick_strategy, ""));
    menu = menu_of_default_menu SushiGo;
  }

let default_game_settings_with_menu n menu =
  { players = List.init n (fun _ -> (first_pick_strategy, "")); menu }

let run_arena game_settings =
  let _ = Subarasushi.Arena.arena game_settings in
  ()

let are_players_sorted l =
  let rec is_sorted l =
    match l with
    | [] | [ _ ] -> true
    | (_, score1) :: (name2, score2) :: t ->
        score1 >= score2 && is_sorted ((name2, score2) :: t)
  in
  is_sorted l

let get_winner = function Single x -> x | Draw l -> List.hd l

let winners_have_same_points = function
  | Single _ -> true
  | Draw l ->
      let _, first_player_score = List.hd l in
      List.for_all (fun (_, score) -> score = first_player_score) l

let menu_breaks_game menu player_number =
  let _, _, _, _, s1, s2, _ = menu in
  player_number > 6
  && (CardType.card_type_of_card (Special s1) = Menu
     || CardType.card_type_of_card (Special s1) = SpecialOrder
     || CardType.card_type_of_card (Special s2) = Menu
     || CardType.card_type_of_card (Special s2) = SpecialOrder)

let win_data_is_accurate =
  let open QCheck in
  Test.make ~count:10000 ~name:"Game ending data is accurate"
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
      if menu_breaks_game game_settings.menu nb_players then true
      else
        let game_ending = arena game_settings in
        let winner_points = get_winner game_ending.winners |> snd in
        are_players_sorted game_ending.players
        && winner_points = (List.hd game_ending.players |> snd)
        && winners_have_same_points game_ending.winners)

let test_strategy ?(min_player_numer = 2) message strategy =
  let open QCheck in
  Test.make ~count:1000 ~name:message
    (pair (int_range min_player_numer 8) arbitrary_menu)
    (fun (nb_players, menu) ->
      let game_settings =
        {
          players =
            List.init nb_players (fun i ->
                (strategy, Printf.sprintf "Player %d" i));
          menu;
        }
      in
      if menu_breaks_game game_settings.menu nb_players || nb_players < 2 then
        true
      else
        let _game_ending = arena game_settings in
        true)

let first_pick_startegy_does_not_brea_the_game =
  test_strategy "First pick strategy does not break the game"
    first_pick_strategy

let hands_are_not_empty_when_copying =
  test_strategy "Hands are not empty when copying"
    mock_test_non_empty_hand_when_copying

let options_are_not_empty =
  test_strategy "Options are not empty" mock_test_non_empty_options

let correct_number_of_turns =
  let open QCheck in
  Test.make ~count:100 ~name:"Number of turns is correct"
    (pair (int_range 2 8) arbitrary_menu)
    (fun (nb_players, menu) ->
      ref_total_turns := 0;
      let game_settings =
        {
          players =
            List.init nb_players (fun i ->
                (mock_test_number_turns, Printf.sprintf "Player %d" i));
          menu;
        }
      in
      if menu_breaks_game game_settings.menu nb_players then true
      else
        let _game_ending = arena game_settings in
        !ref_total_turns = nb_players * 3 * number_of_cards_to_deal ~nb_players)

let playing_invalid_cards =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Playing invalid cards asks the player to retry n times"
    (triple (int_range 2 8) (int_range 1 10) arbitrary_menu)
    (fun (nb_players, number_of_tries, menu) ->
      ref_total_turns := 0;
      let game_settings =
        {
          players =
            List.init nb_players (fun i ->
                ( mock_always_plays_impossible_cards,
                  Printf.sprintf "Player %d" i ));
          menu;
        }
      in
      if menu_breaks_game game_settings.menu nb_players then true
      else
        let _game_ending = arena ~number_of_tries game_settings in
        !ref_total_turns
        = nb_players * 3 * number_of_tries * number_of_cards_to_deal ~nb_players)

(** The minimum possible score, which is extremely hard to achieve, is -25. 
    You must use the menu Temaki, Eel, Sashimi, Miso, Chopsticks, TakeOutBox and Fruit.*)
let points_are_never_lower_than_the_minimun_possible_score =
  let open QCheck in
  Test.make ~count:1000
    ~name:"Points are never lower than the minimum possible score (-25)"
    (pair (int_range 2 8) arbitrary_menu)
    (fun (nb_players, menu) ->
      ref_total_turns := 0;
      let game_settings =
        {
          players =
            List.init nb_players (fun i ->
                (first_pick_strategy, Printf.sprintf "Player %d" i));
          menu;
        }
      in
      if menu_breaks_game game_settings.menu nb_players then true
      else
        let game_ending = arena game_settings in
        List.nth game_ending.players (nb_players - 1) |> snd >= -25)

let decompose_player player = (player.name, player.score)

let _default_internal_game_status menu nb_players strategy =
  {
    players =
      List.init nb_players (fun i ->
          {
            player = default_named_player (Printf.sprintf "Player %d" i);
            hand = [];
            strategy;
          });
    deck = create_deck menu;
    menu;
    played_uramakis = 0;
    current_turn = 1;
    current_round = 1;
    total_special_order_copying_desserts = 0;
  }

let default_internal_game_status menu players =
  {
    players;
    deck = create_deck menu;
    menu;
    played_uramakis = 0;
    current_turn = 1;
    current_round = 1;
    total_special_order_copying_desserts = 0;
  }

let players_from_hand_list strategy hand_list =
  List.mapi
    (fun i hand ->
      let player = default_named_player "Bot" in
      { player = { player with id = i }; hand; strategy })
    hand_list

let players_from_hand_strat hand_strat_list =
  List.mapi
    (fun i (hand, strat) ->
      let player = default_named_player "Bot" in
      { player = { player with id = i }; hand; strategy = strat })
    hand_strat_list

let test_card_is_played_next_turn menu card strategy =
  let hands : card list list =
    [
      [ Nigiri Egg; Nigiri Salmon; Nigiri Squid; card ];
      [ Nigiri Egg; Appetizer Tempura; Dessert Pudding; Special SpecialOrder ];
    ]
  in
  let players = players_from_hand_list strategy hands in
  let internal_game_status =
    default_internal_game_status menu players |> play_turn ~number_of_tries:3
  in
  let result =
    try
      let _ = play_turn ~number_of_tries:3 internal_game_status in
      false
    with MockTestException _ -> true
  in
  Alcotest.(check bool) "same" true result

let test_card_is_at_pos_i ?(turn_amount = 1) pos card strat =
  let hands : card list list =
    [
      [ Nigiri Egg; Nigiri Egg; card; Nigiri Egg ];
      [ Nigiri Egg; Nigiri Egg; Nigiri Egg; Nigiri Egg ];
      [ Nigiri Egg; Nigiri Egg; Nigiri Egg; Nigiri Egg ];
    ]
  in
  let game_status =
    players_from_hand_list strat hands
    |> default_internal_game_status (menu_of_default_menu SushiGo)
  in
  let game_status =
    List.init turn_amount (fun _ -> ())
    |> List.fold_left
         (fun game_status _ -> play_turn ~number_of_tries:3 game_status)
         game_status
  in
  Alcotest.(check bool)
    "same" true
    (List.mem card (List.nth game_status.players pos).hand)

let test_spoon_choices_are_always_of_same_type_but_different =
  let mock_play_spoon =
    {
      first_pick_strategy with
      choose_card = (mock_prioritize_one_card (Special (Spoon 4))).choose_card;
      play_spoon = (fun _ _ -> Some (Generic Nigiri));
      choose_card_to_give =
        (fun _ _ ~options ->
          match options with
          | [] -> failwith "Empty options"
          | h :: t ->
              if
                List.for_all
                  (fun card ->
                    CardType.card_type_of_card h
                    = CardType.card_type_of_card card)
                  t
              then
                if partition_list ( = ) (h :: t) |> List.length > 1 then h
                else raise (MockTestException "All cards are equal")
              else raise (MockTestException "Not all cards are of same type"));
    }
  in
  test_strategy ~min_player_numer:3
    "Spoon choices are always of same type but have atleast 2 different values"
    mock_play_spoon

let turn_play_test ?(turn_amount = 1) menu hand_strat
    (test_function : player list -> unit) =
  let game_status =
    hand_strat |> players_from_hand_strat |> default_internal_game_status menu
  in
  List.init turn_amount (fun _ -> ())
  |> List.fold_left
       (fun game_status _ -> play_turn ~number_of_tries:3 game_status)
       game_status
  |> fun x ->
  x.players |> List.map (fun player -> player.player) |> test_function

let turn_play_miso_test ?(turn_amount = 1) hand_strat expected_hands =
  List.iter2
    (fun hand player ->
      Alcotest.(check bool)
        "contains same elements"
        (contain_same_elements hand player.table)
        true)
    expected_hands
  |> turn_play_test ~turn_amount (menu_of_default_menu DinnerForTwo) hand_strat

let test_play_misosoup_card_informations_menu =
  (Maki 0, Tempura, Sashimi, Tofu, Menu 0, SoySauce, Fruit [])

let test_play_misosoup_card_players =
  List.init 2 (fun _ ->
      [
        Appetizer MisoSoup;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
      ])
  |> players_from_hand_list first_pick_strategy

let test_play_misosoup_card_internal_game_status =
  default_internal_game_status test_play_misosoup_card_informations_menu
    test_play_misosoup_card_players

let use_miso_soup_card () =
  Alcotest.test_case "When I play a miso soup nothing happens" `Quick (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let new_game_status =
           let rec repeat game_status n =
             let () = Format.printf "%a" pp_internal_game_status game_status in
             match n with
             | 0 -> game_status
             | n -> repeat (play_turn ~number_of_tries:20 game_status) (n - 1)
           in
           repeat test_play_misosoup_card_internal_game_status 9
         in
         let () = Format.printf "%a" pp_internal_game_status new_game_status in
         List.for_all (fun p -> List.length p.hand == 0) new_game_status.players))

let run_test_take_out message strat =
  let open QCheck in
  Test.make ~count:1000 ~name:message
    (pair (int_range 2 8) arbitrary_menu)
    (fun (nb_players, menu) ->
      let game_settings =
        {
          players =
            List.init nb_players (fun i ->
                (strat, Printf.sprintf "Player %d" i));
          menu;
        }
      in
      if menu_breaks_game game_settings.menu nb_players then true
      else
        let _ = arena game_settings in
        true)

let hand_size_reduces_after_each_turn =
  let open QCheck in
  Test.make ~count:10000 ~name:"Hand size reduces after each turn"
    (arbitrary_internal_game_status first_pick_strategy) (fun game_status ->
      let original_hand_size =
        List.hd game_status.players |> fun player -> List.length player.hand
      in
      let new_game_status = play_turn ~number_of_tries:3 game_status in
      List.for_all
        (fun player -> List.length player.hand = original_hand_size - 1)
        new_game_status.players)

let for_all_i f l =
  let rec fai i = function [] -> true | h :: t -> f i h && fai (i + 1) t in
  fai 0 l

let test_pass_hands =
  let open QCheck in
  Test.make ~count:1000 ~name:"Players give correctly their hands"
    (arbitrary_internal_game_status first_pick_strategy) (fun game_status ->
      let origin_table = List.map (fun p -> p.hand) game_status.players in
      let passed_table =
        List.map (fun p -> p.hand) (pass_hands game_status.players)
      in
      match List.rev origin_table with
      | [] -> true
      | h :: t ->
          List.hd passed_table = h
          && for_all_i
               (fun i x -> x = List.nth passed_table (i + 1))
               (List.rev t))

let test_generator_internal_game_status =
  let open QCheck in
  Test.make ~count:10000 ~name:"Generator generates properly"
    (arbitrary_internal_game_status first_pick_strategy) (fun game_status ->
      let hand_size =
        number_of_cards_to_deal ~nb_players:(List.length game_status.players)
        - game_status.current_turn + 1
      in
      let menu_list = card_type_list_of_menu game_status.menu in
      List.for_all
        (fun player ->
          List.length player.hand = hand_size
          && List.for_all (fun x -> is_in_menu x menu_list) player.hand
          && List.for_all (fun x -> is_in_menu x menu_list) player.player.table)
        game_status.players)

let face_down_can_face_dow_all_cards =
  let mock_take_out_box = mock_prioritize_one_card_type CardType.TakeOutBox in
  let mock_take_out_box =
    {
      mock_take_out_box with
      choose_cards_to_flip = (fun _ player -> player.table);
    }
  in
  let open QCheck in
  Test.make ~count:10000 ~name:"Face down all cards does so"
    (arbitrary_internal_game_status mock_take_out_box) (fun game_status ->
      let new_game_status = play_turn ~number_of_tries:4 game_status in
      List.for_all2
        (fun player old_player ->
          if
            List.exists
              (function Special (TakeOutBox _) -> true | _ -> false)
              old_player.hand
          then
            List.for_all
              (function FaceDown _ -> true | _ -> false)
              player.player.table
            && List.length player.player.table
               = List.length old_player.player.table
          else true)
        new_game_status.players game_status.players)

let is_card_instant_placed = function
  | Special (Menu _)
  | Special SpecialOrder
  | Special (TakeOutBox _)
  | Appetizer MisoSoup
  | SushiRoll (Uramaki _)
  | Dessert _ ->
      false
  | _ -> true

let test_card_placed_implies_not_in_hand =
  (* I am so sorry, I am becoming a monster *)
  let ref_cards_to_play = ref [] in
  let keep_track_of_playing_cards =
    {
      first_pick_strategy with
      choose_card =
        (fun game_status player_status ->
          let card_to_play =
            first_pick_strategy.choose_card game_status player_status
          in
          ref_cards_to_play := card_to_play :: !ref_cards_to_play;
          card_to_play);
    }
  in
  let open QCheck in
  Test.make ~count:10000 ~name:"Card placed implies not in hand"
    (arbitrary_internal_game_status
       ~menu:(menu_of_default_menu MasterMenu)
       keep_track_of_playing_cards)
    (fun game_status ->
      ref_cards_to_play := [];
      let game_status =
        {
          game_status with
          players =
            List.map
              (fun p -> { p with player = { p.player with table = [] } })
              game_status.players;
        }
      in
      let next_game_status = play_turn ~number_of_tries:3 game_status in
      List.for_all2
        (fun played_card player ->
          let player_table = player.player.table in
          if is_card_instant_placed played_card then
            List.mem played_card player_table
          else true)
        (!ref_cards_to_play |> List.rev)
        next_game_status.players)

let test_copied_hand_is_placed =
  (* This should be illegal, I am afraid of what I have become *)
  let cards_to_copy = Array.init 8 (fun _ -> []) in
  let keep_track_of_copied_cards =
    {
      first_pick_strategy with
      choose_card_to_copy =
        (fun game_status player ->
          let copy =
            first_pick_strategy.choose_card_to_copy game_status player
          in
          cards_to_copy.(player.id) <- copy :: cards_to_copy.(player.id);
          copy);
    }
  in
  let open QCheck in
  Test.make ~count:10000 ~name:"Card copied is placed"
    (arbitrary_internal_game_status keep_track_of_copied_cards)
    (fun game_status ->
      List.init 8 (fun i -> i) |> List.iter (fun i -> cards_to_copy.(i) <- []);
      let next_game_status = play_turn ~number_of_tries:3 game_status in
      List.for_all2
        (fun copied_cards player ->
          let player_table = player.player.table in
          if copied_cards = [] then true
          else
            List.for_all (fun card -> List.mem card player_table) copied_cards)
        (Array.to_list cards_to_copy
        |> List.filteri (fun i _ -> i < List.length game_status.players))
        next_game_status.players)

let table_is_empty_on_turn_1 =
  let mock_assert_empty_hand =
    {
      first_pick_strategy with
      choose_card =
        (fun game_status player_status ->
          if game_status.current_turn = 1 then
            if player_status.player.table = [] then
              first_pick_strategy.choose_card game_status player_status
            else raise (MockTestException "Hand is not empty")
          else first_pick_strategy.choose_card game_status player_status);
    }
  in
  test_strategy "Table is empty on turn 1" mock_assert_empty_hand

let hand_is_full_on_turn_1 =
  let mock_assert_full_hand =
    {
      first_pick_strategy with
      choose_card =
        (fun game_status player_status ->
          if game_status.current_turn = 1 then
            if
              List.length player_status.hand
              = number_of_cards_to_deal
                  ~nb_players:(List.length game_status.players)
            then first_pick_strategy.choose_card game_status player_status
            else raise (MockTestException "Hand is not full")
          else first_pick_strategy.choose_card game_status player_status);
    }
  in
  test_strategy "Hand is full on turn 1" mock_assert_full_hand

let test_play_special_menu_card_informations_menu =
  (Maki 0, Tempura, Sashimi, Tofu, Menu 0, SoySauce, Fruit [])

let test_play_special_menu_card_players =
  List.init 2 (fun i ->
      [
        Special (Menu (7 + i));
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
        Appetizer Tempura;
      ])
  |> players_from_hand_list first_pick_strategy

let test_play_special_menu_card_internal_game_status =
  default_internal_game_status test_play_special_menu_card_informations_menu
    test_play_special_menu_card_players

let use_menu_card () =
  Alcotest.test_case
    "The special menu card does not have a card after being played" `Quick
    (fun () ->
      Alcotest.(check bool)
        "same result" true
        (let new_game_status =
           play_turn ~number_of_tries:20
             test_play_special_menu_card_internal_game_status
         in
         List.for_all
           (fun p ->
             let p = p.player in
             List.length p.desserts + List.length p.table = 1)
           new_game_status.players))

let () =
  let open Alcotest in
  run "Arena"
    [
      ("use_menu_card", [ use_menu_card () ]);
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
          test_case "Menu card on a 7 player game raises an exception" `Quick
            (fun () ->
              Alcotest.check_raises "7 players raises an exception"
                (Invalid_argument
                   "Invalid menu : Menu should not be selected with 7 or 8 \
                    players") (fun () ->
                  run_arena
                    (default_game_settings_with_menu 7
                       (menu_of_default_menu PartySampler))));
          test_case "Menu card on a 8 player game raises an exception" `Quick
            (fun () ->
              Alcotest.check_raises "8 players raises an exception"
                (Invalid_argument
                   "Invalid menu : Menu should not be selected with 7 or 8 \
                    players") (fun () ->
                  run_arena
                    (default_game_settings_with_menu 8
                       (menu_of_default_menu PartySampler))));
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
              let expected = Draw (List.map decompose_player players) in
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
              let expected =
                Draw
                  ([ List.nth players 0; List.nth players 1 ]
                  |> List.map decompose_player)
              in
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
              let expected = Single (decompose_player (List.nth players 2)) in
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
              let expected = Single (decompose_player (List.nth players 2)) in
              let actual = game_ending_of_player_list players in
              Alcotest.(check testable_win) "same" expected actual.winners);
        ] );
      ( "First pick strategy",
        [
          QCheck_alcotest.to_alcotest first_pick_startegy_does_not_brea_the_game;
        ] );
      ( "Hands are not empty when copying",
        [ QCheck_alcotest.to_alcotest hands_are_not_empty_when_copying ] );
      ( "Options are not empty",
        [ QCheck_alcotest.to_alcotest options_are_not_empty ] );
      ( "Number of turns played is correct",
        [ QCheck_alcotest.to_alcotest correct_number_of_turns ] );
      ( "Points are never lower than the minimum possible score (-25)",
        [
          QCheck_alcotest.to_alcotest
            points_are_never_lower_than_the_minimun_possible_score;
        ] );
      ( "Playing invalid cards",
        [ QCheck_alcotest.to_alcotest playing_invalid_cards ] );
      ( "Chopsticks",
        [
          test_case "Playing chopsticks means that you can play them next turn"
            `Quick (fun () ->
              test_card_is_played_next_turn
                (menu_of_default_menu SushiGo)
                (Special (Chopsticks 3)) mock_assert_chopstics_call);
          test_case "When using chopstics, they go to the oponent's hands"
            `Quick (fun () ->
              let chop_strat =
                mock_prioritize_one_card (Special (Chopsticks 1))
              in
              {
                chop_strat with
                play_chopsticks = (fun _ _ -> Some (Nigiri Egg));
              }
              |> test_card_is_at_pos_i ~turn_amount:2 1 (Special (Chopsticks 1)));
        ] );
      ( "Spoon",
        [
          test_case "Playing spoon means that you can play them next turn"
            `Quick (fun () ->
              test_card_is_played_next_turn
                (menu_of_default_menu MasterMenu)
                (Special (Spoon 4)) mock_assert_spoon_call);
          test_case "When using spoon, they go to the oponent's hands" `Quick
            (fun () ->
              let chop_strat = mock_prioritize_one_card (Special (Spoon 4)) in
              {
                chop_strat with
                play_spoon = (fun _ _ -> Some (Generic Nigiri));
              }
              |> test_card_is_at_pos_i ~turn_amount:2 2 (Special (Spoon 4)));
          QCheck_alcotest.to_alcotest
            test_spoon_choices_are_always_of_same_type_but_different;
        ] );
      ( "Miso Soup",
        [
          test_case
            "Miso soup wants to be alone (if 2 or more player play it then it  \
             disapears :) )"
            `Quick (fun () ->
              turn_play_miso_test
                [
                  ( [ Appetizer MisoSoup; Nigiri Egg; Appetizer MisoSoup ],
                    mock_prioritize_one_card (Appetizer MisoSoup) );
                  ( [
                      Appetizer Tofu; Appetizer MisoSoup; Dessert MatchaIceCream;
                    ],
                    mock_prioritize_one_card (Appetizer MisoSoup) );
                ]
                [ []; [] ]);
          use_miso_soup_card ();
        ] );
      ( "TakeOutBox",
        [
          {
            first_pick_strategy with
            choose_cards_to_flip = (fun _ player -> player.table);
          }
          |> run_test_take_out "Flipping all cards"
          |> QCheck_alcotest.to_alcotest;
          { first_pick_strategy with choose_cards_to_flip = (fun _ _ -> []) }
          |> run_test_take_out "Flipping no cards"
          |> QCheck_alcotest.to_alcotest;
          {
            first_pick_strategy with
            choose_cards_to_flip =
              (fun _ player -> list_random_elements player.table);
          }
          |> run_test_take_out "Flipping random subset"
          |> QCheck_alcotest.to_alcotest;
          QCheck_alcotest.to_alcotest face_down_can_face_dow_all_cards;
          test_case "face down empty table" `Quick (fun () ->
              let game_status =
                [
                  ([ Special (TakeOutBox 12) ], first_pick_strategy);
                  ([ Nigiri Egg ], first_pick_strategy);
                ]
                |> players_from_hand_strat
                |> default_internal_game_status
                     (menu_of_default_menu MasterMenu)
              in
              let new_game_status = play_turn ~number_of_tries:3 game_status in
              Alcotest.(check (list card_testable))
                "same" [] (List.nth new_game_status.players 0).player.table;
              Alcotest.(check (list card_testable))
                "same" [ Nigiri Egg ]
                (List.nth new_game_status.players 1).player.table);
        ] );
      ( "SpecialOrder",
        [ QCheck_alcotest.to_alcotest test_copied_hand_is_placed ] );
      ( "General game test",
        [
          QCheck_alcotest.to_alcotest hand_size_reduces_after_each_turn;
          QCheck_alcotest.to_alcotest test_card_placed_implies_not_in_hand;
          QCheck_alcotest.to_alcotest table_is_empty_on_turn_1;
          QCheck_alcotest.to_alcotest hand_is_full_on_turn_1;
        ] );
      ( "Game generator",
        [ QCheck_alcotest.to_alcotest test_generator_internal_game_status ] );
      (" Pass hands", [ QCheck_alcotest.to_alcotest test_pass_hands ]);
    ]
