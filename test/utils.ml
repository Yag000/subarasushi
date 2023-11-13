open Subarasushi.Cards
open Subarasushi.Player
open Subarasushi.Arena
open Subarasushi.Utils
open Subarasushi.Display

let deck =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_deck)) equal_deck

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

let generator_special =
  let open QCheck in
  Gen.oneof
    [
      Gen.oneof
        [
          Gen.return (Chopsticks 1);
          Gen.return (Chopsticks 2);
          Gen.return (Chopsticks 3);
        ];
      Gen.oneof
        [ Gen.return (Menu 7); Gen.return (Menu 8); Gen.return (Menu 9) ];
      Gen.return SoySauce;
      Gen.oneof
        [ Gen.return (Spoon 4); Gen.return (Spoon 5); Gen.return (Spoon 6) ];
      Gen.return SpecialOrder;
      Gen.oneof
        [
          Gen.return (TakeOutBox 10);
          Gen.return (TakeOutBox 11);
          Gen.return (TakeOutBox 12);
        ];
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

let arbitrary_player =
  QCheck.make ~print:(Format.asprintf "%a" pp_player) generator_player

let testable_win =
  let open Alcotest in
  testable (Fmt.of_to_string (Format.asprintf "%a" pp_win)) equal_win
