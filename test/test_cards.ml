open Subarasushi.Cards
open Subarasushi.Utils
open Alcotest
open Utils

let run_round_test_case name expected result =
  test_case name `Quick (fun () ->
      Alcotest.(check int) "same result" expected result)

let players_cards = [ (2, 10); (3, 10); (4, 9); (5, 9); (6, 8); (7, 8); (8, 7) ]

let run_create_deck_test_case (name, expected_deck, menu) =
  let open Alcotest in
  test_case name `Quick (fun () ->
      Alcotest.(check deck_deconstructed_testable)
        "same deck" expected_deck
        (deck_deconstruct (create_deck menu)))

let run_shuffle_cards_tests =
  let open QCheck in
  Test.make ~count:100
    ~name:"test shuffle cards"
      (* This function will test if this statement is true for different values. *)
    (list arbitrary_card) (fun l -> contain_same_elements l (shuffle_cards l))

let run_card_type_of_card_test_case (name, expected_type, card) =
  let open Alcotest in
  test_case name `Quick (fun () ->
      Alcotest.(check card_type_testable)
        "same CardType" expected_type
        (CardType.card_type_of_card card))

let run_menu_of_default_menu_test_case (name, expected_menu, default) =
  let open Alcotest in
  test_case name `Quick (fun () ->
      Alcotest.(check menu_deconstructed_testable)
        "same menu"
        (menu_deconstruct expected_menu)
        (menu_deconstruct (menu_of_default_menu default)))

let deal_cards_preserves_elements deck (handl, undistributed) _nb_players _round
    =
  let dck = deck_deconstruct deck in
  let undis = deck_deconstruct undistributed in
  let cards = fst dck in
  let hand_desserts =
    List.flatten handl
    |> List.filter_map (function Dessert x -> Some x | _ -> None)
  in
  let deck_desserts =
    List.filter_map (function Dessert x -> Some x | _ -> None) (fst undis)
  in
  let total_desserts = hand_desserts @ deck_desserts in
  let list_repartition =
    List.flatten handl @ fst undis
    |> List.filter (function Dessert _ -> false | _ -> true)
  in
  contain_same_elements cards list_repartition
  &&
  match List.hd total_desserts with
  | MatchaIceCream -> List.for_all (fun x -> x = MatchaIceCream) total_desserts
  | Pudding -> List.for_all (fun x -> x = Pudding) total_desserts
  | Fruit _ -> includes fruit_list total_desserts

let run_deal_cards_test_case (name, deck, nb_players, round) =
  let open Alcotest in
  test_case name `Quick (fun () ->
      Alcotest.(check bool)
        "same repartition" true
        (deal_cards_preserves_elements deck
           (deal_cards deck ~nb_players ~round)
           nb_players round))

let run_deal_cards_tests (name, deck) =
  List.init 7 (fun i -> i + 2)
  |> List.fold_left
       (fun acc nb_players ->
         List.init 3 (fun j -> j + 1)
         |> List.fold_left
              (fun acc round ->
                let test_case =
                  run_deal_cards_test_case
                    ( name ^ " P : " ^ string_of_int nb_players ^ "; R :"
                      ^ string_of_int round,
                      deck,
                      nb_players,
                      round )
                in
                test_case :: acc)
              acc)
       []
  |> List.rev

let () =
  let open Alcotest in
  run "Cards"
    [
      ( "create_deck",
        [
          run_create_deck_test_case
            ( "MyFirstMeal test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Special (Wasabi None);
                  Special (Wasabi None);
                  Special (Wasabi None);
                  Special Tea;
                  Special Tea;
                  Special Tea;
                ],
                MatchaIceCream ),
              menu_of_default_menu MyFirstMeal );
          run_create_deck_test_case
            ( "SushiGo test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Special (Chopsticks 1);
                  Special (Chopsticks 2);
                  Special (Chopsticks 3);
                  Special (Wasabi None);
                  Special (Wasabi None);
                  Special (Wasabi None);
                ],
                Pudding ),
              menu_of_default_menu SushiGo );
          run_create_deck_test_case
            ( "PartySampler test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Special (Wasabi None);
                  Special (Wasabi None);
                  Special (Wasabi None);
                  Special (Menu 7);
                  Special (Menu 8);
                  Special (Menu 9);
                ],
                MatchaIceCream ),
              menu_of_default_menu PartySampler );
          run_create_deck_test_case
            ( "MasterMenu test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Circle);
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Special (Spoon 4);
                  Special (Spoon 5);
                  Special (Spoon 6);
                  Special (TakeOutBox 10);
                  Special (TakeOutBox 11);
                  Special (TakeOutBox 12);
                ],
                Fruit [] ),
              menu_of_default_menu MasterMenu );
          run_create_deck_test_case
            ( "PointsPlatter test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Circle);
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Special SpecialOrder;
                  Special SpecialOrder;
                  Special SpecialOrder;
                  Special Tea;
                  Special Tea;
                  Special Tea;
                ],
                MatchaIceCream ),
              menu_of_default_menu PointsPlatter );
          run_create_deck_test_case
            ( "CutthroatCombo test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  SushiRoll Temaki;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Special (Spoon 4);
                  Special (Spoon 5);
                  Special (Spoon 6);
                  Special SoySauce;
                  Special SoySauce;
                  Special SoySauce;
                ],
                Pudding ),
              menu_of_default_menu CutthroatCombo );
          run_create_deck_test_case
            ( "BigBanquet test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Tempura;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Special (Spoon 4);
                  Special (Spoon 5);
                  Special (Spoon 6);
                  Special (Chopsticks 1);
                  Special (Chopsticks 2);
                  Special (Chopsticks 3);
                ],
                MatchaIceCream ),
              menu_of_default_menu BigBanquet );
          run_create_deck_test_case
            ( "DinnerForTwo test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  SushiRoll (Uramaki 3);
                  SushiRoll (Uramaki 4);
                  SushiRoll (Uramaki 5);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Circle);
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Appetizer MisoSoup;
                  Special (Menu 7);
                  Special (Menu 8);
                  Special (Menu 9);
                  Special SpecialOrder;
                  Special SpecialOrder;
                  Special SpecialOrder;
                ],
                Fruit [] ),
              menu_of_default_menu DinnerForTwo );
          run_create_deck_test_case
            ( "Custom Menu test",
              ( [
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Squid;
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Tofu;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Appetizer Sashimi;
                  Special (Spoon 4);
                  Special (Spoon 5);
                  Special (Spoon 6);
                  Special Tea;
                  Special Tea;
                  Special Tea;
                ],
                Pudding ),
              custom_menu_example );
        ] );
      ("shuffle_cards", [ QCheck_alcotest.to_alcotest run_shuffle_cards_tests ]);
      ( "card_type_of_card",
        [
          run_card_type_of_card_test_case ("Nigiri Egg test", Nigiri, Nigiri Egg);
          run_card_type_of_card_test_case
            ("Nigiri Salmon test", Nigiri, Nigiri Salmon);
          run_card_type_of_card_test_case
            ("Nigiri Squid test", Nigiri, Nigiri Squid);
          run_card_type_of_card_test_case
            ("SushiRoll Maki test", Maki, SushiRoll (Maki 0));
          run_card_type_of_card_test_case
            ("SushiRoll Uramaki test", Uramaki, SushiRoll (Uramaki 0));
          run_card_type_of_card_test_case
            ("SushiRoll Temaki test", Temaki, SushiRoll Temaki);
          run_card_type_of_card_test_case
            ("Appetizer Dumpling test", Dumpling, Appetizer Dumpling);
          run_card_type_of_card_test_case
            ("Appetizer Edamame test", Edamame, Appetizer Edamame);
          run_card_type_of_card_test_case
            ("Appetizer Eel test", Eel, Appetizer Eel);
          run_card_type_of_card_test_case
            ("Appetizer Onigiri test", Onigiri, Appetizer (Onigiri Circle));
          run_card_type_of_card_test_case
            ("Appetizer MisoSoup test", MisoSoup, Appetizer MisoSoup);
          run_card_type_of_card_test_case
            ("Appetizer Sashimi test", Sashimi, Appetizer Sashimi);
          run_card_type_of_card_test_case
            ("Appetizer Tempura test", Tempura, Appetizer Tempura);
          run_card_type_of_card_test_case
            ("Appetizer Tofu test", Dumpling, Appetizer Dumpling);
          run_card_type_of_card_test_case
            ("Special Chopsticks test", Chopsticks, Special (Chopsticks 0));
          run_card_type_of_card_test_case
            ("Special Spoon test", Spoon, Special (Spoon 0));
          run_card_type_of_card_test_case
            ("Special Menu test", Menu, Special (Menu 0));
          run_card_type_of_card_test_case
            ("Special SoySauce test", SoySauce, Special SoySauce);
          run_card_type_of_card_test_case
            ("Special SpecialOrder test", SpecialOrder, Special SpecialOrder);
          run_card_type_of_card_test_case
            ("Special Wasabi test", Wasabi, Special (Wasabi None));
          run_card_type_of_card_test_case
            ( "Dessert MatchaIceCream test",
              MatchaIceCream,
              Dessert MatchaIceCream );
          run_card_type_of_card_test_case
            ("Dessert Fruit test", Fruit, Dessert (Fruit []));
          run_card_type_of_card_test_case
            ("Dessert Pudding test", Pudding, Dessert Pudding);
          run_card_type_of_card_test_case
            ("FaceDown Nigiri Egg test", Nigiri, FaceDown (Nigiri Egg));
          run_card_type_of_card_test_case
            ("FaceDown Nigiri Salmon test", Nigiri, FaceDown (Nigiri Salmon));
          run_card_type_of_card_test_case
            ("FaceDown Nigiri Squid test", Nigiri, FaceDown (Nigiri Squid));
          run_card_type_of_card_test_case
            ("FaceDown SushiRoll Maki test", Maki, FaceDown (SushiRoll (Maki 0)));
          run_card_type_of_card_test_case
            ( "FaceDown SushiRoll Uramaki test",
              Uramaki,
              FaceDown (SushiRoll (Uramaki 0)) );
          run_card_type_of_card_test_case
            ( "FaceDown SushiRoll Temaki test",
              Temaki,
              FaceDown (SushiRoll Temaki) );
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer Dumpling test",
              Dumpling,
              FaceDown (Appetizer Dumpling) );
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer Edamame test",
              Edamame,
              FaceDown (Appetizer Edamame) );
          run_card_type_of_card_test_case
            ("FaceDown Appetizer Eel test", Eel, FaceDown (Appetizer Eel));
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer Onigiri test",
              Onigiri,
              FaceDown (Appetizer (Onigiri Circle)) );
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer MisoSoup test",
              MisoSoup,
              FaceDown (Appetizer MisoSoup) );
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer Sashimi test",
              Sashimi,
              FaceDown (Appetizer Sashimi) );
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer Tempura test",
              Tempura,
              FaceDown (Appetizer Tempura) );
          run_card_type_of_card_test_case
            ( "FaceDown Appetizer Tofu test",
              Dumpling,
              FaceDown (Appetizer Dumpling) );
          run_card_type_of_card_test_case
            ( "FaceDown Special Chopsticks test",
              Chopsticks,
              FaceDown (Special (Chopsticks 0)) );
          run_card_type_of_card_test_case
            ("FaceDown Special Spoon test", Spoon, FaceDown (Special (Spoon 0)));
          run_card_type_of_card_test_case
            ("FaceDown Special Menu test", Menu, FaceDown (Special (Menu 0)));
          run_card_type_of_card_test_case
            ( "FaceDown Special SoySauce test",
              SoySauce,
              FaceDown (Special SoySauce) );
          run_card_type_of_card_test_case
            ( "FaceDown Special SpecialOrder test",
              SpecialOrder,
              FaceDown (Special SpecialOrder) );
          run_card_type_of_card_test_case
            ( "FaceDown Special Wasabi test",
              Wasabi,
              FaceDown (Special (Wasabi None)) );
          run_card_type_of_card_test_case
            ( "FaceDown Dessert MatchaIceCream test",
              MatchaIceCream,
              FaceDown (Dessert MatchaIceCream) );
          run_card_type_of_card_test_case
            ("FaceDown Dessert Fruit test", Fruit, FaceDown (Dessert (Fruit [])));
          run_card_type_of_card_test_case
            ( "FaceDown Dessert Pudding test",
              Pudding,
              FaceDown (Dessert Pudding) );
        ] );
      ( "menu_of_default_menu",
        [
          run_menu_of_default_menu_test_case
            ( "MyFirstMeal test",
              ( Maki 0,
                Tempura,
                Sashimi,
                MisoSoup,
                Wasabi None,
                Tea,
                MatchaIceCream ),
              MyFirstMeal );
          run_menu_of_default_menu_test_case
            ( "SushiGo test",
              ( Maki 0,
                Tempura,
                Sashimi,
                Dumpling,
                Chopsticks 0,
                Wasabi None,
                Pudding ),
              SushiGo );
          run_menu_of_default_menu_test_case
            ( "PartySampler test",
              ( Temaki,
                Tempura,
                Dumpling,
                Tofu,
                Wasabi None,
                Menu 0,
                MatchaIceCream ),
              PartySampler );
          run_menu_of_default_menu_test_case
            ( "MasterMenu test",
              ( Temaki,
                Onigiri Circle,
                Tofu,
                Sashimi,
                Spoon 0,
                TakeOutBox 0,
                Fruit [] ),
              MasterMenu );
          run_menu_of_default_menu_test_case
            ( "PointsPlatter test",
              ( Uramaki 0,
                Onigiri Circle,
                Dumpling,
                Edamame,
                SpecialOrder,
                Tea,
                MatchaIceCream ),
              PointsPlatter );
          run_menu_of_default_menu_test_case
            ( "CutthroatCombo test",
              (Temaki, Eel, Tofu, MisoSoup, Spoon 0, SoySauce, Pudding),
              CutthroatCombo );
          run_menu_of_default_menu_test_case
            ( "BigBanquet test",
              ( Maki 0,
                Tempura,
                Dumpling,
                Eel,
                Spoon 0,
                Chopsticks 0,
                MatchaIceCream ),
              BigBanquet );
          run_menu_of_default_menu_test_case
            ( "DinnerForTwo test",
              ( Uramaki 0,
                Onigiri Circle,
                Tofu,
                MisoSoup,
                Menu 0,
                SpecialOrder,
                Fruit [] ),
              DinnerForTwo );
        ] );
      ( "deal_cards MyFirstMeal",
        run_deal_cards_tests ("MyFirstMeal", deck_MyFirstMeal) );
      ("deal_cards SushiGo", run_deal_cards_tests ("SushiGo", deck_SushiGo));
      ( "deal_cards PartySampler",
        run_deal_cards_tests ("PartySampler", deck_PartySampler) );
      ( "deal_cards MasterMenu",
        run_deal_cards_tests ("MasterMenu", deck_MasterMenu) );
      ( "deal_cards PointsPlatter",
        run_deal_cards_tests ("PointsPlatter", deck_PointsPlatter) );
      ( "deal_cards CutthroatCombo",
        run_deal_cards_tests ("CutthroatCombo", deck_CutthroatCombo) );
      ( "deal_cards BigBanquet",
        run_deal_cards_tests ("BigBanquet", deck_BigBanquet) );
      ( "deal_cards DinnerForTwo",
        run_deal_cards_tests ("DinnerForTwo", deck_DinnerForTwo) );
      ( "deal_cards CustomMenu",
        run_deal_cards_tests ("CustomMenu", create_deck custom_menu_example) );
      ( "Number Of Cards",
        List.map
          (fun (p, c) ->
            run_round_test_case
              (Format.sprintf "Players : %i | Cards : %i" p c)
              c
              (number_of_cards_to_deal ~nb_players:p))
          players_cards );
    ]
