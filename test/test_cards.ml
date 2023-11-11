open Subarasushi.Cards
open Alcotest

let run_round_test_case name expected result =
  test_case name `Quick (fun () ->
      Alcotest.(check int) "same result" expected result)

let players_cards = [ (2, 10); (3, 10); (4, 9); (5, 9); (6, 8); (7, 8); (8, 7) ]

let () =
  run "Cards"
    [
      ( "Number Of Cards",
        List.map
          (fun (p, c) ->
            run_round_test_case
              (Format.sprintf "Players : %i | Cards : %i" p c)
              c
              (number_of_cards_to_deal ~nb_players:p))
          players_cards );
      ("", []);
    ]
