open Subarasushi.Cards
open Subarasushi.Points
open Subarasushi.Player

let points_from_players = List.map (fun p -> p.score)

let compute_round_points_with_uramaki played_uramakis hands =
  let players =
    List.map
      (fun cards -> { name = "test"; table = cards; score = 0; desserts = [] })
      hands
  in
  count_round_points ~played_uramakis players |> points_from_players

let compute_round_points = compute_round_points_with_uramaki 0

(** Runs a test case for the [compute_round_points] function *)
let run_round_test_case (name, expected_points, hands) =
  let open Alcotest in
  test_case name `Quick (fun () ->
      Alcotest.(check (list int))
        "same result" expected_points
        (compute_round_points hands))

let players_from_desserts =
  List.map (fun cards ->
      { name = "test"; table = []; score = 0; desserts = cards })

let compute_desserts_points desserts =
  desserts |> players_from_desserts |> count_dessert_points
  |> points_from_players

(** Runs a test case for the [compute_desserts_points] function *)
let run_dessert_test_case (name, expected_points, desserts) =
  let open Alcotest in
  test_case name `Quick (fun () ->
      Alcotest.(check (list int))
        "same result" expected_points
        (compute_desserts_points desserts))

let () =
  let open Alcotest in
  run "Points"
    [
      ( "Empty hands",
        [
          run_round_test_case ("Empty hands in the list", [ 0; 0 ], [ []; [] ]);
          run_round_test_case
            ("Empty hands in the list", [ 0; 0; 0 ], [ []; []; [] ]);
        ] );
      ( "Nigiris",
        [
          run_round_test_case
            ( "One nigiri",
              [ 1; 3; 2 ],
              [ [ Nigiri Egg ]; [ Nigiri Squid ]; [ Nigiri Salmon ] ] );
          run_round_test_case
            ( "Multiple nigiris",
              [ 6; 0; 10 ],
              [
                [ Nigiri Squid; Nigiri Salmon; Nigiri Egg ];
                [];
                [
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Egg;
                ];
              ] );
          run_round_test_case
            ( "Maximun number of players",
              [ 6; 0; 3; 0; 2; 0; 0; 1 ],
              [
                [ Nigiri Squid; Nigiri Squid ];
                [];
                [ Nigiri Salmon; Nigiri Egg ];
                [];
                [ Nigiri Salmon ];
                [];
                [];
                [ Nigiri Egg ];
              ] );
        ] );
      ( "Makis",
        [
          run_round_test_case
            ("1 winner | 2 players", [ 0; 6 ], [ []; [ SushiRoll (Maki 2) ] ]);
          run_round_test_case
            ( "1 winner | 3 players",
              [ 0; 6; 0 ],
              [ []; [ SushiRoll (Maki 2) ]; [] ] );
          run_round_test_case
            ( "1 winner | 6 players",
              [ 0; 6; 0; 0; 0; 0 ],
              [ []; [ SushiRoll (Maki 2) ]; []; []; []; [] ] );
          run_round_test_case
            ( "2 winners | 3 players",
              [ 0; 6; 6 ],
              [ []; [ SushiRoll (Maki 2) ]; [ SushiRoll (Maki 2) ] ] );
          run_round_test_case
            ( "All winners | 3 players",
              [ 6; 6; 6 ],
              [
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
              ] );
          run_round_test_case
            ( "Two winners, 1 second | 3 players",
              [ 3; 6; 6 ],
              [
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
              ] );
          run_round_test_case
            ( "2 winners, 1 second | 6 players",
              [ 0; 6; 6; 0; 0; 4 ],
              [
                [];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
                [];
                [];
                [ SushiRoll (Maki 1) ];
              ] );
          run_round_test_case
            ( "1 winners, 2 second | 6 players",
              [ 0; 4; 6; 2; 0; 4 ],
              [
                [];
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 3) ];
              ] );
          run_round_test_case
            ( "1 winner, 5 seconds | 6 players",
              [ 6; 4; 4; 4; 4; 4 ],
              [
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 1) ];
              ] );
          run_round_test_case
            ( "3 winners, 1 second, 1 third | 6 players",
              [ 6; 6; 6; 0; 4; 2 ],
              [
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 3) ];
                [];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 1) ];
              ] );
          run_round_test_case
            ( "1 winner, 1 second, 1 third | 6 players",
              [ 0; 0; 6; 4; 0; 2 ],
              [
                [];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 1); SushiRoll (Maki 1); SushiRoll (Maki 1) ];
                [];
                [ SushiRoll (Maki 2) ];
              ] );
          run_round_test_case
            ( "1 winner, 1 second, 2 thirds | 6 players",
              [ 2; 0; 6; 4; 0; 2 ],
              [
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 1) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3) ];
                [];
                [ SushiRoll (Maki 2) ];
              ] );
          run_round_test_case
            ( "2 winner, 2 second, 2 thirds | 6 players",
              [ 2; 4; 6; 6; 4; 2 ],
              [
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 2) ];
              ] );
          run_round_test_case
            ( "2 winner, 2 second, 2 thirds | 7 players",
              [ 2; 4; 6; 6; 4; 0; 2 ],
              [
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3) ];
                [];
                [ SushiRoll (Maki 2) ];
              ] );
          run_round_test_case
            ( "1 winner, 1 second, 4 thirds | 6 players",
              [ 2; 2; 6; 4; 2; 2 ],
              [
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3); SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 3) ];
                [ SushiRoll (Maki 2) ];
                [ SushiRoll (Maki 2) ];
              ] );
        ] );
      ( "Temakis",
        [
          run_round_test_case
            ("1 winner | 2 players", [ 0; 4 ], [ []; [ SushiRoll Temaki ] ]);
          run_round_test_case
            ( "1 winner 2 loosers | 3 players",
              [ -4; 4; -4 ],
              [ []; [ SushiRoll Temaki ]; [] ] );
          run_round_test_case
            ( "1 winner 2 loosers | 3 players",
              [ -4; 4; 0 ],
              [
                []; [ SushiRoll Temaki; SushiRoll Temaki ]; [ SushiRoll Temaki ];
              ] );
          run_round_test_case
            ( "2 winners, 3 losers | 6 players",
              [ -4; 4; 4; -4; -4; 0 ],
              [
                [];
                [ SushiRoll Temaki; SushiRoll Temaki ];
                [ SushiRoll Temaki; SushiRoll Temaki ];
                [];
                [];
                [ SushiRoll Temaki ];
              ] );
        ] );
      ( "Uramakis",
        [
          test_case "1 winner, 0 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 0; 8; 0 ]
                (compute_round_points_with_uramaki 0
                   [ []; [ SushiRoll (Uramaki 1) ]; [] ]));
          test_case "1 winner, 1 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 0; 5; 0 ]
                (compute_round_points_with_uramaki 1
                   [ []; [ SushiRoll (Uramaki 1) ]; [] ]));
          test_case "1 winner, 2 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 0; 2; 0 ]
                (compute_round_points_with_uramaki 2
                   [ []; [ SushiRoll (Uramaki 1) ]; [] ]));
          test_case "1 winner, 3 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 0; 0; 0 ]
                (compute_round_points_with_uramaki 3
                   [ []; [ SushiRoll (Uramaki 1) ]; [] ]));
          test_case "2 winners, 0 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 8; 8; 0 ]
                (compute_round_points_with_uramaki 0
                   [ [ SushiRoll (Uramaki 1) ]; [ SushiRoll (Uramaki 1) ]; [] ]));
          test_case "2 winners, 1 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 5; 0; 5 ]
                (compute_round_points_with_uramaki 1
                   [ [ SushiRoll (Uramaki 1) ]; []; [ SushiRoll (Uramaki 1) ] ]));
          test_case "2 winners, 2 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 0; 2; 2 ]
                (compute_round_points_with_uramaki 2
                   [ []; [ SushiRoll (Uramaki 1) ]; [ SushiRoll (Uramaki 1) ] ]));
          test_case "2 winners, 3 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 0; 0; 0 ]
                (compute_round_points_with_uramaki 3
                   [ [ SushiRoll (Uramaki 1) ]; [ SushiRoll (Uramaki 1) ]; [] ]));
          test_case "2 winners, 2 losers, 0 played | 4 players" `Quick
            (fun () ->
              Alcotest.(check (list int))
                "same result" [ 8; 8; 0; 0 ]
                (compute_round_points_with_uramaki 0
                   [
                     [ SushiRoll (Uramaki 5) ];
                     [ SushiRoll (Uramaki 5) ];
                     [ SushiRoll (Uramaki 2) ];
                     [ SushiRoll (Uramaki 1) ];
                   ]));
          test_case "1 winner, 1 loser, 1 played | 3 players" `Quick (fun () ->
              Alcotest.(check (list int))
                "same result" [ 5; 0; 0 ]
                (compute_round_points_with_uramaki 1
                   [ [ SushiRoll (Uramaki 3) ]; [ SushiRoll (Uramaki 1) ]; [] ]));
        ] );
      ( "Dumpling",
        [
          run_round_test_case
            ( "Dumpling | 7 players, [|0, 6|] Dumplings",
              [ 0; 1; 3; 6; 10; 15; 15 ],
              List.init 7 (fun i -> List.init i (fun _ -> Appetizer Dumpling))
            );
        ] );
      ( "Sashimi",
        [
          run_round_test_case
            ( "Sashimi | 8 players, [|0, 7|] Sashimis",
              [ 0; 0; 0; 10; 10; 10; 20; 20 ],
              List.init 8 (fun i -> List.init i (fun _ -> Appetizer Sashimi)) );
        ] );
      ( "Tempura",
        [
          run_round_test_case
            ( "Tempura | 8 players, [|0, 7|] Tempuras",
              [ 0; 0; 5; 5; 10; 10; 15; 15 ],
              List.init 8 (fun i -> List.init i (fun _ -> Appetizer Tempura)) );
        ] );
      ( "Eel",
        [
          run_round_test_case
            ( "Eel | 5 players, [|0, 4|] Eels",
              [ 0; -3; 7; 7; 7 ],
              List.init 5 (fun i -> List.init i (fun _ -> Appetizer Eel)) );
        ] );
      ( "Miso Soup",
        [
          run_round_test_case
            ( "Miso Soup | 5 players, [|0, 4|] Miso Soups",
              [ 0; 3; 6; 9; 12 ],
              List.init 5 (fun i -> List.init i (fun _ -> Appetizer MisoSoup))
            );
        ] );
      ( "Tofu",
        [
          run_round_test_case
            ( "Tofu | 6 players, [|0, 5|] Tofus",
              [ 0; 2; 6; 0; 0; 0 ],
              List.init 6 (fun i -> List.init i (fun _ -> Appetizer Tofu)) );
        ] );
      ( "Onigiri",
        [
          (* For this test I use the following convention. The tuples make reference to the two sets of
             different [onigiri] shapes. For example (3,1) means that a player has a set of 3 different [onigiri] shapes
             and another one with only one shape, e.g. (Circle, Circle, Square, Triangle) *)
          run_round_test_case
            ( "1, 0 | 2 players",
              [ 0; 1 ],
              [ []; [ Appetizer (Onigiri Circle) ] ] );
          run_round_test_case
            ( "2, 0 | 2 players",
              [ 0; 4 ],
              [
                []; [ Appetizer (Onigiri Circle); Appetizer (Onigiri Triangle) ];
              ] );
          run_round_test_case
            ( "3, 0 | 2 players",
              [ 9; 0 ],
              [
                [
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Square);
                ];
                [];
              ] );
          run_round_test_case
            ( "4, 0 | 2 players",
              [ 0; 16 ],
              [
                [];
                [
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Rectangle);
                ];
              ] );
          run_round_test_case
            ( "4, 1 | 2 players",
              [ 0; 17 ],
              [
                [];
                [
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Rectangle);
                ];
              ] );
          run_round_test_case
            ( "4, 2 | 2 players",
              [ 0; 20 ],
              [
                [];
                [
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Triangle);
                ];
              ] );
          run_round_test_case
            ( "4, 3 | 2 players",
              [ 0; 25 ],
              [
                [];
                [
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Triangle);
                ];
              ] );
          run_round_test_case
            ( "4, 4 | 2 players",
              [ 0; 32 ],
              [
                [];
                [
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Triangle);
                ];
              ] );
        ] );
      ( "Edamamme",
        [
          run_round_test_case
            ( "1 played it | 5 players",
              [ 0; 0; 0; 0; 0 ],
              [ []; [ Appetizer Edamame; Appetizer Edamame ]; []; []; [] ] );
          run_round_test_case
            ( "2 played it | 5 players",
              [ 0; 0; 2; 1; 0 ],
              [
                [];
                [];
                [ Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame ];
                [];
              ] );
          run_round_test_case
            ( "3 played it | 5 players",
              [ 0; 4; 4; 2; 0 ],
              [
                [];
                [ Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame ];
                [];
              ] );
          run_round_test_case
            ( "4 played it | 5 players",
              [ 12; 9; 6; 3; 0 ],
              [
                [
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                ];
                [ Appetizer Edamame; Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame ];
                [];
              ] );
          run_round_test_case
            ( "5 played it | 5 players",
              [ 16; 12; 8; 4; 20 ],
              [
                [
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                ];
                [ Appetizer Edamame; Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame ];
                [
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                ];
              ] );
          (* This test case shows that the maximum amount of points a edamame card can give is 4 *)
          run_round_test_case
            ( "6 played it | 6 players",
              [ 16; 12; 8; 4; 20; 4 ],
              [
                [
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                ];
                [ Appetizer Edamame; Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame; Appetizer Edamame ];
                [ Appetizer Edamame ];
                [
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                  Appetizer Edamame;
                ];
                [ Appetizer Edamame ];
              ] );
        ] );
      ( "Worthless specials",
        [
          run_round_test_case
            ("Chopsticks", [ 0; 0 ], [ []; [ Special (Chopsticks 2) ] ]);
          run_round_test_case ("Menu", [ 0; 0 ], [ []; [ Special (Menu 2) ] ]);
          run_round_test_case ("Spoon", [ 0; 0 ], [ [ Special (Spoon 2) ]; [] ]);
          run_round_test_case
            ("Special order", [ 0; 0 ], [ []; [ Special SpecialOrder ] ]);
          run_round_test_case
            ("TakeOutBox", [ 0; 0 ], [ []; [ Special (TakeOutBox 1) ] ]);
          run_round_test_case
            ( "All together",
              [ 0; 0; 0 ],
              [
                [ Special (Spoon 1) ];
                [
                  Special (TakeOutBox 1);
                  Special (Chopsticks 2);
                  Special (Menu 2);
                  Special (Spoon 2);
                  Special SpecialOrder;
                ];
                [ Special (Chopsticks 2); Special SpecialOrder ];
              ] );
        ] );
      ( "Tea",
        [
          run_round_test_case
            ( "Only tea | 3 players",
              [ 1; 9; 4 ],
              [
                [ Special Tea ];
                [ Special Tea; Special Tea; Special Tea ];
                [ Special Tea; Special Tea ];
              ] );
          run_round_test_case
            ( "2 background colors | 3 players",
              [ 2; 4; 2 ],
              [
                [ Special Tea; Nigiri Egg ];
                [
                  Special Tea;
                  Special Tea;
                  Special (Chopsticks 1);
                  Special (Chopsticks 2);
                ];
                [ Nigiri Salmon ];
              ] );
          run_round_test_case
            ( "Face down cards have no color | 3 players",
              [ 7; 2; 0 ],
              [
                [
                  Special Tea;
                  FaceDown (Nigiri Egg);
                  FaceDown (Appetizer Dumpling);
                  FaceDown (Appetizer MisoSoup);
                ];
                [ FaceDown (Special Tea) ];
                [];
              ] );
          run_round_test_case
            ( "Wasabi and nigiri have same color | 3 Players",
              [ 11; 0; 0 ],
              [
                [
                  Special Tea;
                  Nigiri Egg;
                  Special (Wasabi (Some Egg));
                  Nigiri Squid;
                ];
                [];
                [];
              ] );
        ] );
      ( "Wasabi",
        [
          run_round_test_case
            ( "Empty wasabi | 3 players",
              [ 0; 0; 0 ],
              [
                [ Special (Wasabi None); Special (Wasabi None) ];
                [ Special (Wasabi None) ];
                [];
              ] );
          run_round_test_case
            ( "Wasabi | 4 playres",
              [ 3; 6; 9; 18 ],
              [
                [ Special (Wasabi (Some Egg)) ];
                [ Special (Wasabi (Some Salmon)) ];
                [ Special (Wasabi (Some Squid)) ];
                [
                  Special (Wasabi (Some Egg));
                  Special (Wasabi (Some Salmon));
                  Special (Wasabi (Some Squid));
                ];
              ] );
        ] );
      ( "Soy sauce",
        [
          run_round_test_case
            ( "Winner no contestants | 3 players",
              [ 9; 1; 2 ],
              [
                [ Special SoySauce; Special SoySauce; Nigiri Egg ];
                [ Nigiri Egg ];
                [ Nigiri Salmon ];
              ] );
          run_round_test_case
            ( "Looser no contestants | 3 players",
              [ 1; 1; 2 ],
              [
                [
                  Special SoySauce;
                  Special SoySauce;
                  Special SoySauce;
                  Nigiri Egg;
                ];
                [ Nigiri Egg; Special (Chopsticks 2); Dessert Pudding ];
                [ Nigiri Salmon ];
              ] );
          run_round_test_case
            ( "Winner and looser | 3 players",
              [ 5; 0; 2 ],
              [
                [ Special SoySauce; Nigiri Egg; Special (Chopsticks 2) ];
                [ Special (Spoon 2); Special SoySauce ];
                [ Appetizer Tofu ];
              ] );
          run_round_test_case
            ( "2 winners | 3 players",
              [ 5; 11; 2 ],
              [
                [ Special SoySauce; Nigiri Egg; Special (Chopsticks 2) ];
                [
                  Special (Spoon 2);
                  Special SoySauce;
                  Special SoySauce;
                  Appetizer MisoSoup;
                ];
                [ Appetizer Tofu ];
              ] );
        ] );
      ( "TakeOut Face Down",
        [
          run_round_test_case
            ( "FaceDown | 5 players",
              [ 10; 0; 4; 2; 6 ],
              [
                [
                  FaceDown (Nigiri Salmon);
                  FaceDown (Nigiri Squid);
                  FaceDown (Appetizer MisoSoup);
                  FaceDown (Dessert Pudding);
                  FaceDown (Appetizer Sashimi);
                ];
                [];
                [ FaceDown (Appetizer Tempura); FaceDown (Appetizer Tempura) ];
                [ FaceDown (Nigiri Squid) ];
                [
                  FaceDown (Dessert Pudding);
                  FaceDown (Dessert Pudding);
                  FaceDown (Dessert Pudding);
                ];
              ] );
        ] );
      ( "3 player game",
        (* This cases come from an actual Sushi Go Party game *)
        [
          run_round_test_case
            ( "Round 1",
              [ 20; 20; 11 ],
              [
                [
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 1);
                  SushiRoll (Maki 1);
                  Dessert Pudding;
                  Appetizer Eel;
                  Appetizer Eel;
                  Nigiri Squid;
                  Nigiri Squid;
                  Nigiri Egg;
                ];
                [
                  Special (Spoon 5);
                  Nigiri Egg;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Eel;
                  Appetizer Eel;
                  SushiRoll (Maki 3);
                ];
                [
                  Special (Spoon 4);
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Nigiri Egg;
                  Nigiri Squid;
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Rectangle);
                  Dessert Pudding;
                  Dessert Pudding;
                  Dessert Pudding;
                ];
              ] );
          run_round_test_case
            ( "Round 2",
              [ 23; 17; 26 ],
              [
                [
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 1);
                  Nigiri Salmon;
                  Special (Spoon 5);
                ];
                [
                  Dessert Pudding;
                  Dessert Pudding;
                  Appetizer (Onigiri Square);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Appetizer Eel;
                  Appetizer Eel;
                  Appetizer Eel;
                  Nigiri Egg;
                ];
                [
                  Special (Spoon 6);
                  Nigiri Squid;
                  Nigiri Egg;
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Triangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Square);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                ];
              ] );
          run_round_test_case
            ( "Round 3",
              [ 18; 20; 14 ],
              [
                [
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  Appetizer Dumpling;
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 1);
                  Nigiri Squid;
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Nigiri Egg;
                  Appetizer (Onigiri Rectangle);
                ];
                [
                  Appetizer Eel;
                  Appetizer Eel;
                  Dessert Pudding;
                  Appetizer (Onigiri Rectangle);
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Triangle);
                  Nigiri Salmon;
                  Nigiri Salmon;
                  Special (Spoon 4);
                  SushiRoll (Maki 1);
                ];
                [
                  Special (Spoon 5);
                  Appetizer Dumpling;
                  Nigiri Squid;
                  Appetizer (Onigiri Circle);
                  Appetizer (Onigiri Square);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 3);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 2);
                  SushiRoll (Maki 1);
                ];
              ] );
        ] );
      ( "Mactha Ice Cream",
        [
          run_dessert_test_case
            ( "Matcha Ice Cream | 2 players, [|0, 1|] Matcha teas",
              [ 0; 0 ],
              [ []; [ MatchaIceCream ] ] );
          run_dessert_test_case
            ( "Matcha Ice Cream | 8 players, [|1, 8|] Matcha teas",
              [ 0; 0; 0; 12; 12; 12; 12; 24 ],
              List.init 8 (fun i -> List.init (i + 1) (fun _ -> MatchaIceCream))
            );
        ] );
      ( "Pudding",
        [
          run_dessert_test_case
            ( "One winner | 2 players",
              [ 0; 6 ],
              [ [ Pudding ]; [ Pudding; Pudding ] ] );
          run_dessert_test_case
            ( "1 winner, 1 loser | 3 players",
              [ 0; -6; 6 ],
              [ [ Pudding ]; []; [ Pudding; Pudding ] ] );
          run_dessert_test_case
            ( "3 winners, 2 loosers | 6 players",
              [ 6; 0; 6; -6; -6; 6 ],
              [
                [ Pudding; Pudding ];
                [ Pudding ];
                [ Pudding; Pudding ];
                [];
                [];
                [ Pudding; Pudding ];
              ] );
        ] );
      ( "Fruits",
        [
          run_dessert_test_case
            ( "Win 1 fruit type, Lose 3 fruit types | 2 players",
              [ -3; -6 ],
              [ [ Fruit [ Watermelon; Watermelon ] ]; [] ] );
          run_dessert_test_case
            ( "Both players have at least 5 of each fruit | 2 players",
              [ 30; 30 ],
              [
                List.init 3 (fun _ -> Fruit [ Watermelon; Watermelon ])
                @ List.init 3 (fun _ -> Fruit [ Pineapple; Pineapple ])
                @ List.init 3 (fun _ -> Fruit [ Orange; Orange ]);
                List.init 7 (fun _ -> Fruit [ Watermelon; Watermelon ])
                @ List.init 7 (fun _ -> Fruit [ Pineapple; Pineapple ])
                @ List.init 7 (fun _ -> Fruit [ Orange; Orange ]);
              ] );
          run_dessert_test_case
            ( "Mixed values | 5 players",
              [ 1; 1; 3; 8; 9 ],
              [
                [
                  Fruit [ Watermelon; Watermelon ]; Fruit [ Watermelon; Orange ];
                ];
                [
                  Fruit [ Watermelon; Orange ]; Fruit [ Watermelon; Pineapple ];
                ];
                [
                  Fruit [ Watermelon; Orange ];
                  Fruit [ Watermelon; Pineapple ];
                  Fruit [ Pineapple; Orange ];
                ];
                [
                  Fruit [ Pineapple; Pineapple ];
                  Fruit [ Pineapple; Pineapple ];
                  Fruit [ Pineapple; Watermelon ];
                ];
                [
                  Fruit [ Pineapple; Pineapple ];
                  Fruit [ Pineapple; Pineapple ];
                  Fruit [ Watermelon; Watermelon ];
                  Fruit [ Watermelon; Orange ];
                ];
              ] );
        ] );
    ]
