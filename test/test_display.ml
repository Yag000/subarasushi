open Subarasushi.Display
open Subarasushi.Cards
open Subarasushi.Player
open Subarasushi.Arena
open Utils

let test_pp (expected : string) (result : string) =
  Alcotest.test_case expected `Quick (fun () ->
      Alcotest.(check string) "same result" expected result)

let test_empty_hand =
  test_pp "Title: []" (Format.asprintf "%a" (pp_titled_card_list "Title") [])

let test_nigiris =
  test_pp "Title: [NiEgg; NiSlm; NiSqd]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       [ Nigiri Egg; Nigiri Salmon; Nigiri Squid ])

let test_wasabis =
  test_pp "Title: [Wasabi; Wsb NiEgg; Wsb NiSlm; Wsb NiSqd]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       [
         Special (Wasabi None);
         Special (Wasabi (Some Egg));
         Special (Wasabi (Some Salmon));
         Special (Wasabi (Some Squid));
       ])

let test_sushi_rolls =
  test_pp
    "Title: [Temaki; MakiRo 1; MakiRo 2; MakiRo 3; UrMaki 3; UrMaki 4; UrMaki \
     5]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       ([
          [ SushiRoll Temaki ];
          List.init 3 (fun i -> SushiRoll (Maki (i + 1)));
          List.init 3 (fun i -> SushiRoll (Uramaki (i + 3)));
        ]
       |> List.flatten))

let test_appetizers =
  test_pp
    "Title: [Dmplng; Edamam; Eel; MisoSp; Sashmi; Tmpura; Tofu; OniCir; OniTri;\n\
    \        OniSqr; OniRec]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       ([
          [ Appetizer Dumpling ];
          [ Appetizer Edamame ];
          [ Appetizer Eel ];
          [ Appetizer MisoSoup ];
          [ Appetizer Sashimi ];
          [ Appetizer Tempura ];
          [ Appetizer Tofu ];
          [
            Appetizer (Onigiri Circle);
            Appetizer (Onigiri Triangle);
            Appetizer (Onigiri Square);
            Appetizer (Onigiri Rectangle);
          ];
        ]
       |> List.flatten))

let test_specials =
  test_pp
    "Title: [TkoBox 10; TkoBox 11; TkoBox 12; Menu 7; Menu 8; Menu 9; Spoon 4;\n\
    \        Spoon 5; Spoon 6; Chpstk 1; Chpstk 2; Chpstk 3; SoySce; SpOrdr; \
     Tea;\n\
    \        Wasabi]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       ([
          List.init 3 (fun i -> Special (TakeOutBox (i + 10)));
          List.init 3 (fun i -> Special (Menu (i + 7)));
          List.init 3 (fun i -> Special (Spoon (i + 4)));
          List.init 3 (fun i -> Special (Chopsticks (i + 1)));
          [ Special SoySauce ];
          [ Special SpecialOrder ];
          [ Special Tea ];
          [ Special (Wasabi None) ];
        ]
       |> List.flatten))

let test_fruits =
  test_pp
    "Title: [Fruit: [O; O]; Fruit: [O; W]; Fruit: [O; P]; Fruit: [W; W];\n\
    \        Fruit: [W; P]; Fruit: [P; P]]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       ([
          [ Dessert (Fruit [ Orange; Orange ]) ];
          [ Dessert (Fruit [ Orange; Watermelon ]) ];
          [ Dessert (Fruit [ Orange; Pineapple ]) ];
          [ Dessert (Fruit [ Watermelon; Watermelon ]) ];
          [ Dessert (Fruit [ Watermelon; Pineapple ]) ];
          [ Dessert (Fruit [ Pineapple; Pineapple ]) ];
        ]
       |> List.flatten))

let test_desserts =
  test_pp "Title: [Matcha; Puddng; Fruit: []]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       ([
          [ Dessert MatchaIceCream ];
          [ Dessert Pudding ];
          [ Dessert (Fruit []) ];
        ]
       |> List.flatten))

let test_facedowns =
  let open QCheck in
  Test.make ~count:100
    ~name:"for all cards, pp_card (FaceDown card) = \"FcDown\"" arbitrary_card
    (fun card -> Format.asprintf "%a" pp_card (FaceDown card) = "FcDown")

let test_mixed_cards =
  test_pp
    "Title: [NiEgg; Wasabi; Wsb NiEgg; Temaki; MakiRo 1; UrMaki 3; Dmplng; \
     OniCir;\n\
    \        TkoBox 10; TkoBox 11; TkoBox 12; SoySce; Fruit: [O; O]; Matcha;\n\
    \        FcDown; FcDown]"
    (Format.asprintf "%a"
       (pp_titled_card_list "Title")
       ([
          [ Nigiri Egg ];
          [ Special (Wasabi None); Special (Wasabi (Some Egg)) ];
          [ SushiRoll Temaki; SushiRoll (Maki 1); SushiRoll (Uramaki 3) ];
          [ Appetizer Dumpling; Appetizer (Onigiri Circle) ];
          List.init 3 (fun i -> Special (TakeOutBox (i + 10)));
          [ Special SoySauce ];
          [ Dessert (Fruit [ Orange; Orange ]) ];
          [ Dessert MatchaIceCream ];
          [ FaceDown (Nigiri Egg); FaceDown (Special (Wasabi (Some Squid))) ];
        ]
       |> List.flatten))

let test_empty_player =
  test_pp "player1 (0 pts)\n    Desserts: []\n    Table: []"
    (Format.asprintf "%a" pp_player (default_named_player "player1"))

let test_custom_player =
  let player =
    {
      name = "Frank";
      id = 0;
      score = 20;
      desserts = [ Pudding; MatchaIceCream; Fruit [ Orange; Pineapple ] ];
      table = [ Nigiri Egg; Special (Wasabi None) ];
    }
  in
  test_pp
    "Frank (20 pts)\n\
    \    Desserts: [Puddng; Matcha; Fruit: [O; P]]\n\
    \    Table: [NiEgg; Wasabi]"
    (Format.asprintf "%a" pp_player player)

let test_partysampler_menu =
  test_pp "Menu : [Nigiri; Temaki; Tmpura; Dmplng; Tofu; Wasabi; Menu; Matcha]"
    (Format.asprintf "%a" pp_menu (menu_of_default_menu PartySampler))

let test_game_status =
  let game_status =
    {
      players =
        [ default_named_player "Robert"; default_named_player "Bonjour" ];
      current_round = 2;
      current_turn = 7;
      menu = menu_of_default_menu SushiGo;
    }
  in
  test_pp
    "Round: 2\n\
     Turn: 7\n\
     Menu : [Nigiri; MakiRo; Tmpura; Sashmi; Dmplng; Chpstk; Wasabi; Puddng]\n\
     Players: [\n\
     Robert (0 pts)\n\
    \    Desserts: []\n\
    \    Table: [];\n\
     Bonjour (0 pts)\n\
    \    Desserts: []\n\
    \    Table: []\n\
     ]"
    (Format.asprintf "%a" pp_game_status game_status)

let test_win_single =
  test_pp "Winner: Goya 64 pts"
    (Format.asprintf "%a" pp_win (Single ("Goya", 64)))

let test_win_draw =
  let winners = [ ("Sinay", 5); ("Tithan", 14) ] in
  test_pp "Winners: [\nSinay 5 pts\nTithan 14 pts\n]"
    (Format.asprintf "%a" pp_win (Draw winners))

let test_game_ending_one_winner =
  let game_ending =
    {
      winners = Single ("Toto", 99);
      players = [ ("Toto", 99); ("Goya", 64); ("Tithan", 14); ("Sinay", 5) ];
    }
  in
  test_pp
    "Winner: Toto 99 pts\n\
     Leaderboard: [\n\
     Toto 99 pts\n\
     Goya 64 pts\n\
     Tithan 14 pts\n\
     Sinay 5 pts\n\
     ]"
    (Format.asprintf "%a" pp_game_ending game_ending)

let test_game_ending_many_winners =
  let game_ending =
    {
      winners = Draw [ ("Toto", 99); ("Goya", 99) ];
      players = [ ("Toto", 99); ("Goya", 99); ("Tithan", 14); ("Sinay", 5) ];
    }
  in
  test_pp
    "Winners: [\n\
     Toto 99 pts\n\
     Goya 99 pts\n\
     ]\n\
     Leaderboard: [\n\
     Toto 99 pts\n\
     Goya 99 pts\n\
     Tithan 14 pts\n\
     Sinay 5 pts\n\
     ]"
    (Format.asprintf "%a" pp_game_ending game_ending)

let test_player_status =
  let player_status =
    {
      player = default_named_player "Binga";
      hand = [ Nigiri Egg; Special (Wasabi None); Special (Wasabi (Some Egg)) ];
    }
  in
  test_pp
    "Binga (0 pts)\n\
    \    Desserts: []\n\
    \    Table: []\n\
    \    Hand: [NiEgg; Wasabi; Wsb NiEgg]"
    (Format.asprintf "%a" pp_player_status player_status)

let () =
  let open Alcotest in
  run "Display"
    [
      ("test_empty_hand", [ test_empty_hand ]);
      ("test_nigiris", [ test_nigiris ]);
      ("test_wasabis", [ test_wasabis ]);
      ("test_sushi_rolls", [ test_sushi_rolls ]);
      ("test_appetizers", [ test_appetizers ]);
      ("test_specials", [ test_specials ]);
      ("test_fruits", [ test_fruits ]);
      ("test_desserts", [ test_desserts ]);
      ("test_facedowns", [ QCheck_alcotest.to_alcotest test_facedowns ]);
      ("test_mixed_cards", [ test_mixed_cards ]);
      ("test_empty_player", [ test_empty_player ]);
      ("test_custom_player", [ test_custom_player ]);
      ("test_partysampler_menu", [ test_partysampler_menu ]);
      ("test_game_status", [ test_game_status ]);
      ("test_win_single", [ test_win_single ]);
      ("test_win_draw", [ test_win_draw ]);
      ("test_game_ending_one_winner", [ test_game_ending_one_winner ]);
      ("test_game_ending_many_winners", [ test_game_ending_many_winners ]);
      ("test_player_status", [ test_player_status ]);
    ]
