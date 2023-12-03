open Subarasushi.Cards
open Subarasushi.Player
open Subarasushi.Arena
open Subarasushi.Utils
open Subarasushi.Display
open Alcotest

let card_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_card)) equal_card

let card_type_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" CardType.pp)) CardType.equal

let sushi_roll_testable =
  testable
    (Fmt.of_to_string (Format.asprintf "%a" pp_sushi_roll))
    equal_sushi_roll

let appetizer_testable =
  testable
    (Fmt.of_to_string (Format.asprintf "%a" pp_appetizer))
    equal_appetizer

let special_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_special)) equal_special

let dessert_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_dessert)) equal_dessert

let list_card_testable = list card_testable

let menu_deconstructed_testable =
  pair sushi_roll_testable
    (pair (list appetizer_testable)
       (pair (list special_testable) dessert_testable))

let deck_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_deck)) equal_deck

let hand_testable =
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_hand)) equal_hand

let deck_deconstructed_testable = pair list_card_testable dessert_testable
let repartition_testable = pair (list hand_testable) deck_testable
let deck_MyFirstMeal = create_deck (menu_of_default_menu MyFirstMeal)
let deck_SushiGo = create_deck (menu_of_default_menu SushiGo)
let deck_PartySampler = create_deck (menu_of_default_menu PartySampler)
let deck_MasterMenu = create_deck (menu_of_default_menu MasterMenu)
let deck_PointsPlatter = create_deck (menu_of_default_menu PointsPlatter)
let deck_CutthroatCombo = create_deck (menu_of_default_menu CutthroatCombo)
let deck_BigBanquet = create_deck (menu_of_default_menu BigBanquet)
let deck_DinnerForTwo = create_deck (menu_of_default_menu DinnerForTwo)

let menu_deconstruct (sr, app1, app2, app3, sp1, sp2, dess) =
  (sr, ([ app1; app2; app3 ], ([ sp1; sp2 ], dess)))

let custom_menu_example = (Maki 0, Eel, Tofu, Sashimi, Spoon 0, Tea, Pudding)

let rec equal eq l1 l2 =
  match (l1, l2) with
  | [], [] -> true
  | [], _ :: _ | _ :: _, [] -> false
  | a1 :: l1, a2 :: l2 -> eq a1 a2 && equal eq l1 l2

let contain_same_elements l1 l2 =
  let foo x y =
    match (x, y) with a, b when a > b -> 1 | a, b when a < b -> -1 | _ -> 0
  in
  equal ( = ) (List.fast_sort foo l1) (List.fast_sort foo l2)

let generator_nigiri =
  let open QCheck in
  Gen.oneof [ Gen.return Squid; Gen.return Salmon; Gen.return Egg ]

let generator_maki =
  let open QCheck in
  Gen.oneof [ Gen.return (Maki 1); Gen.return (Maki 2); Gen.return (Maki 3) ]

let generator_uramaki =
  let open QCheck in
  Gen.oneof
    [ Gen.return (Uramaki 3); Gen.return (Uramaki 4); Gen.return (Uramaki 5) ]

let generator_sushi_roll =
  let open QCheck in
  Gen.oneof [ generator_maki; generator_uramaki; Gen.return Temaki ]

let generator_onigiri =
  let open QCheck in
  Gen.oneof
    [
      Gen.return (Onigiri Square);
      Gen.return (Onigiri Triangle);
      Gen.return (Onigiri Circle);
      Gen.return (Onigiri Rectangle);
    ]

let generator_appetizer =
  let open QCheck in
  Gen.oneof
    [
      Gen.return Dumpling;
      Gen.return Edamame;
      Gen.return Eel;
      generator_onigiri;
      Gen.return MisoSoup;
      Gen.return Sashimi;
      Gen.return Tempura;
      Gen.return Tofu;
    ]

let chopsticks_generator =
  let open QCheck in
  Gen.oneof
    [
      Gen.return (Chopsticks 1);
      Gen.return (Chopsticks 2);
      Gen.return (Chopsticks 3);
    ]

let spoon_generator =
  let open QCheck in
  Gen.oneof [ Gen.return (Spoon 4); Gen.return (Spoon 5); Gen.return (Spoon 6) ]

let menu_card_generator =
  let open QCheck in
  Gen.oneof [ Gen.return (Menu 7); Gen.return (Menu 8); Gen.return (Menu 9) ]

let takeoutbox_generator =
  let open QCheck in
  Gen.oneof
    [
      Gen.return (TakeOutBox 10);
      Gen.return (TakeOutBox 11);
      Gen.return (TakeOutBox 12);
    ]

let generator_special =
  let open QCheck in
  Gen.oneof
    [
      chopsticks_generator;
      menu_card_generator;
      Gen.return SoySauce;
      spoon_generator;
      Gen.return SpecialOrder;
      takeoutbox_generator;
      Gen.return Tea;
      Gen.return (Wasabi None);
    ]

let generator_fruit =
  let open QCheck in
  Gen.oneof
    [
      Gen.return (Fruit [ Orange; Orange ]);
      Gen.return (Fruit [ Orange; Watermelon ]);
      Gen.return (Fruit [ Orange; Pineapple ]);
      Gen.return (Fruit [ Watermelon; Watermelon ]);
      Gen.return (Fruit [ Watermelon; Pineapple ]);
      Gen.return (Fruit [ Pineapple; Pineapple ]);
    ]

let generator_dessert =
  let open QCheck in
  Gen.oneof [ Gen.return Pudding; Gen.return MatchaIceCream; generator_fruit ]

let arbitrary_dessert =
  QCheck.make ~print:(Format.asprintf "%a" pp_dessert) generator_dessert

let generator_card =
  let open QCheck in
  Gen.oneof
    [
      Gen.map (fun n -> Nigiri n) generator_nigiri;
      Gen.map (fun s -> SushiRoll s) generator_sushi_roll;
      Gen.map (fun a -> Appetizer a) generator_appetizer;
      Gen.map (fun s -> Special s) generator_special;
      Gen.map (fun d -> Dessert d) generator_dessert;
    ]

let arbitrary_card =
  QCheck.make ~print:(Format.asprintf "%a" pp_card) generator_card

(** [get_n_different_elements n l] returns a list of [n] elements of [l] that
    are all different. If [l] has less than [n] different elements, it raises an
    exception. 
    @raise Invalid_argument if [l] has less than [n] different elements
*)
let get_n_different_elements n l seed =
  Random.init seed;
  List.init n (fun _ -> ())
  |> List.fold_left
       (fun (acc, l) _ ->
         if l = [] then raise (Invalid_argument "Not enough elements in list")
         else
           let e = list_random_element_opt l |> Option.get in
           (e :: acc, List.filter (fun e' -> e' <> e) l))
       ([], l)
  |> fst

let get_three_different_appetizers seed =
  get_n_different_elements 3
    [
      Dumpling; Edamame; Eel; MisoSoup; Sashimi; Tempura; Tofu; Onigiri Triangle;
    ]
    seed

let get_two_different_specials seed =
  get_n_different_elements 2
    [
      Chopsticks 0;
      Menu 0;
      SoySauce;
      Spoon 0;
      SpecialOrder;
      TakeOutBox 0;
      Tea;
      Wasabi None;
    ]
    seed

let generator_menu : menu QCheck.Gen.t =
  let open QCheck in
  let appetizers_gen =
    Gen.map (fun x -> get_three_different_appetizers x) Gen.int
  in
  let specials_gen = Gen.map (fun x -> get_two_different_specials x) Gen.int in
  Gen.map
    (fun (sushi_roll, appetizers, specials, dessert) ->
      ( sushi_roll,
        List.nth appetizers 0,
        List.nth appetizers 1,
        List.nth appetizers 2,
        List.nth specials 0,
        List.nth specials 1,
        dessert ))
    (Gen.tup4 generator_sushi_roll appetizers_gen specials_gen generator_dessert)

let arbitrary_menu =
  QCheck.make ~print:(Format.asprintf "%a" pp_menu) generator_menu

let generator_player =
  let open QCheck in
  Gen.map (fun (table, desserts) ->
      let player = default_named_player "test" in
      { player with table; desserts })
  @@ Gen.pair
       (Gen.list_size (Gen.int_range 0 10) generator_card)
       (Gen.list_size (Gen.int_range 0 15) generator_dessert)

let generator_player_internal =
  let open QCheck in
  Gen.map (fun (table, desserts) ->
      let player = default_named_player "test" in
      { player with table; desserts })
  @@ Gen.pair
       (Gen.list_size (Gen.return 10) generator_card)
       (Gen.list_size (Gen.int_range 0 15) generator_dessert)

let arbitrary_player =
  QCheck.make ~print:(Format.asprintf "%a" pp_player) generator_player

let testable_win =
  let open Alcotest in
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_win)) equal_win

let card_type_list_of_menu menu =
  let s, a1, a2, a3, s1, s2, d = menu in
  [
    SushiRoll s;
    Appetizer a1;
    Appetizer a2;
    Appetizer a3;
    Special s1;
    Special s2;
    Dessert d;
  ]
  |> List.map CardType.card_type_of_card

let is_in_menu card menu =
  match CardType.card_type_of_card card with
  | Nigiri -> true
  | x -> List.mem x menu

let get_generator card : card QCheck.Gen.t =
  let open QCheck in
  match CardType.card_type_of_card card with
  | CardType.Nigiri -> generator_nigiri |> Gen.map (fun x -> Nigiri x)
  | CardType.Maki -> generator_maki |> Gen.map (fun x -> SushiRoll x)
  | CardType.Uramaki -> generator_uramaki |> Gen.map (fun x -> SushiRoll x)
  | CardType.Temaki -> Gen.return Temaki |> Gen.map (fun x -> SushiRoll x)
  | CardType.Eel -> Gen.return Eel |> Gen.map (fun x -> Appetizer x)
  | CardType.Dumpling -> Gen.return Dumpling |> Gen.map (fun x -> Appetizer x)
  | CardType.Edamame -> Gen.return Edamame |> Gen.map (fun x -> Appetizer x)
  | CardType.MisoSoup -> Gen.return MisoSoup |> Gen.map (fun x -> Appetizer x)
  | CardType.Sashimi -> Gen.return Sashimi |> Gen.map (fun x -> Appetizer x)
  | CardType.Tempura -> Gen.return Tempura |> Gen.map (fun x -> Appetizer x)
  | CardType.Tofu -> Gen.return Tofu |> Gen.map (fun x -> Appetizer x)
  | CardType.Onigiri -> generator_onigiri |> Gen.map (fun x -> Appetizer x)
  | CardType.SoySauce -> Gen.return SoySauce |> Gen.map (fun x -> Special x)
  | CardType.Wasabi -> Gen.return (Wasabi None) |> Gen.map (fun x -> Special x)
  | CardType.Chopsticks -> chopsticks_generator |> Gen.map (fun x -> Special x)
  | CardType.Spoon -> spoon_generator |> Gen.map (fun x -> Special x)
  | CardType.Menu -> menu_card_generator |> Gen.map (fun x -> Special x)
  | CardType.SpecialOrder ->
      Gen.return SpecialOrder |> Gen.map (fun x -> Special x)
  | CardType.TakeOutBox -> takeoutbox_generator |> Gen.map (fun x -> Special x)
  | CardType.Tea -> Gen.return Tea |> Gen.map (fun x -> Special x)
  | CardType.Pudding -> Gen.return Pudding |> Gen.map (fun x -> Dessert x)
  | CardType.MatchaIceCream ->
      Gen.return MatchaIceCream |> Gen.map (fun x -> Dessert x)
  | CardType.Fruit -> generator_fruit |> Gen.map (fun x -> Dessert x)
  | CardType.FaceDown -> failwith "FaceDown card should not be generated"

let card_generator_from_menu menu =
  let open QCheck in
  let sr, a1, a2, a3, s1, s2, d = menu in
  Gen.oneof
    [
      generator_nigiri |> Gen.map (fun x -> Nigiri x);
      get_generator (SushiRoll sr);
      get_generator (Appetizer a1);
      get_generator (Appetizer a2);
      get_generator (Appetizer a3);
      get_generator (Special s1);
      get_generator (Special s2);
      get_generator (Dessert d);
    ]

let generator_internal_game_status strategy menu =
  let open QCheck in
  Gen.map
    (fun ( players,
           hands,
           played_uramakis,
           total_special_order_copying_desserts,
           current_round,
           current_turn ) ->
      let menu_list = card_type_list_of_menu menu in
      let hand_size =
        number_of_cards_to_deal ~nb_players:(List.length players)
        - current_turn + 1
      in
      let min_table_size =
        max
          (number_of_cards_to_deal ~nb_players:(List.length players))
          (List.fold_left
             (fun acc p -> min acc (List.length p.table))
             0 players)
      in
      let players =
        List.mapi
          (fun i p ->
            let table = List.filter (fun c -> is_in_menu c menu_list) p.table in
            let nb_cards_to_remove = List.length table - min_table_size in
            let table =
              List.filteri (fun i _ -> i >= nb_cards_to_remove) table
            in
            let hand = List.nth hands i in
            let nb_cards_to_remove = List.length hand - hand_size in
            let hand = List.filteri (fun i _ -> i >= nb_cards_to_remove) hand in
            { player = { p with table; id = i }; strategy; hand })
          players
      in
      match
        construct_internal_game_status players ~played_uramakis
          ~total_special_order_copying_desserts ~current_round ~current_turn
          menu
      with
      | Some status -> status
      | _ ->
          raise
            (Invalid_argument "The internal game status could not be created"))
    (Gen.tup6
       (Gen.list_size (Gen.int_range 3 6) generator_player)
       (Gen.list_size (Gen.return 8)
          (Gen.list_size (Gen.return 10) (card_generator_from_menu menu)))
       (Gen.int_range 0 3) (Gen.int_range 0 6) (Gen.int_range 1 3)
       (Gen.int_range 1 3))

let pp_internal_game_status ff internal_game_status =
  let players =
    List.map
      (fun p -> p.player)
      (get_players_from_internal_game_status internal_game_status)
  in
  let hands =
    List.map
      (fun p -> p.hand)
      (get_players_from_internal_game_status internal_game_status)
  in
  let game_status = game_status_of_internal_game_status internal_game_status in
  Format.fprintf ff "Current round: %d@." game_status.current_round;
  Format.fprintf ff "Current turn: %d@." game_status.current_turn;
  Format.fprintf ff "players: %a@." pp_player_list players;
  Format.fprintf ff "Hands: [@[<hov>%a@]]@."
    Format.(pp_print_list ~pp_sep:pp_print_space pp_hand)
    hands;
  Format.fprintf ff "Menu: %a@." pp_menu game_status.menu

let arbitrary_internal_game_status ?(menu = menu_of_default_menu PartySampler)
    strategy =
  QCheck.make ~print:(Format.asprintf "%a" pp_internal_game_status)
  @@ generator_internal_game_status strategy menu
