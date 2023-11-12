open Subarasushi.Arena
open Subarasushi.Strategies
open Utils

let init_game player_number strategy menu =
  {
    players =
      List.init player_number (fun i -> (strategy, Printf.sprintf "player%d" i));
    menu;
  }

let test_random_player_strategy =
  let open QCheck in
  Test.make ~count:200
    ~name:
      "for all menus, the random player strategy should return a valid playing \
       style"
    (pair (int_range 2 8) arbitrary_menu)
    (fun (n, menu) ->
      let _ = init_game n random_player menu |> arena in
      true)

let () =
  Random.self_init ();
  let open Alcotest in
  run "Strategies"
    [
      ( "random_player",
        [ QCheck_alcotest.to_alcotest test_random_player_strategy ] );
    ]
