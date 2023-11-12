type nigiri = Egg | Salmon | Squid [@@deriving show, eq]
type sushi_roll = Maki of int | Temaki | Uramaki of int [@@deriving show, eq]
type shape = Circle | Triangle | Square | Rectangle [@@deriving show, eq]
type fruit_kind = Watermelon | Orange | Pineapple [@@deriving show, eq]

type appetizer =
  | Dumpling
  | Edamame
  | Eel
  | Onigiri of shape
  | MisoSoup
  | Sashimi
  | Tempura
  | Tofu
[@@deriving show, eq]

type special =
  | Chopsticks of int
  | Spoon of int
  | Menu of int
  | SoySauce
  | SpecialOrder
  | TakeOutBox of int
  | Tea
  | Wasabi of nigiri option
[@@deriving show, eq]

type dessert = MatchaIceCream | Fruit of fruit_kind list | Pudding
[@@deriving show, eq]

type card =
  | Nigiri of nigiri
  | SushiRoll of sushi_roll
  | Appetizer of appetizer
  | Special of special
  | Dessert of dessert
  | FaceDown of card
[@@deriving show, eq]

type default_menu =
  | MyFirstMeal
  | SushiGo
  | PartySampler
  | MasterMenu
  | PointsPlatter
  | CutthroatCombo
  | BigBanquet
  | DinnerForTwo

type menu =
  sushi_roll * appetizer * appetizer * appetizer * special * special * dessert
[@@deriving show]

let menu_of_default_menu = function
  | MyFirstMeal ->
      (Maki 0, Tempura, Sashimi, MisoSoup, Wasabi None, Tea, MatchaIceCream)
  | SushiGo ->
      (Maki 0, Tempura, Sashimi, Dumpling, Chopsticks 0, Wasabi None, Pudding)
  | PartySampler ->
      (Temaki, Tempura, Dumpling, Tofu, Wasabi None, Menu 0, MatchaIceCream)
  | MasterMenu ->
      (Temaki, Onigiri Circle, Tofu, Sashimi, Spoon 0, TakeOutBox 0, Fruit [])
  | PointsPlatter ->
      ( Uramaki 0,
        Onigiri Circle,
        Dumpling,
        Edamame,
        SpecialOrder,
        Tea,
        MatchaIceCream )
  | CutthroatCombo -> (Temaki, Eel, Tofu, MisoSoup, Spoon 0, SoySauce, Pudding)
  | BigBanquet ->
      (Maki 0, Tempura, Dumpling, Eel, Spoon 0, Chopsticks 0, MatchaIceCream)
  | DinnerForTwo ->
      (Uramaki 0, Onigiri Circle, Tofu, MisoSoup, Menu 0, SpecialOrder, Fruit [])

type deck = card list * dessert [@@deriving show, eq]
type hand = card list [@@deriving show]

(** Check that all [appetizer]s and [special]s are different from each other *)
let is_well_built (_, app1, app2, app3, spec1, spec2, _) =
  app1 <> app2 && app1 <> app3 && app2 <> app3 && spec1 <> spec2

let maki_ditribution = function
  | index when index < 0 -> raise (Invalid_argument "Negative argument")
  | index when index < 4 -> 1
  | index when index < 9 -> 2
  | _ -> 3

let nigiri_distribution = function Egg -> 4 | Salmon -> 5 | Squid -> 3

(** Call match_sushi_roll (Maki _) will return [Maki 1; Maki 1; Maki 1; Maki 2; Maki 2; ...; Maki 3] (or a permutation of it) for a total of 12 cards *)
let match_sushi_roll = function
  | Temaki -> List.init 12 (fun _ -> SushiRoll Temaki)
  | Maki _ ->
      List.init 12 (fun index -> SushiRoll (Maki (maki_ditribution index)))
  | Uramaki _ ->
      List.init 12 (fun index -> SushiRoll (Uramaki ((index mod 3) + 3)))

let match_special special index =
  match special with
  | Chopsticks _ -> Chopsticks (index + 1)
  | Spoon _ -> Spoon (index + 4)
  | Menu _ -> Menu (index + 7)
  | SoySauce -> SoySauce
  | SpecialOrder -> SpecialOrder
  | TakeOutBox _ -> TakeOutBox (index + 10)
  | Tea -> Tea
  | Wasabi _ -> Wasabi None

let match_appetizer = function
  | Onigiri _ ->
      [ Circle; Triangle; Square; Rectangle ]
      |> List.fold_left
           (fun acc x -> List.init 2 (fun _ -> Appetizer (Onigiri x)) @ acc)
           []
  | appetizer -> List.init 8 (fun _ -> Appetizer appetizer)

(** Browse a list of elements and produce the list of all possible pairs 
    Example with a list of elements : [a1; a2; a3]
    [[a1; a1]; [a1; a2]; [a1; a3]] @ [[a2; a2]; [a2; a3]] @ [[a3; a3]] @ []
 *)
let rec from_n_choose_2 elements =
  match elements with
  | [] -> []
  | element :: rest ->
      List.map (fun x -> (x, element)) elements @ from_n_choose_2 rest

let match_dessert = function
  | MatchaIceCream -> List.init 15 (fun _ -> MatchaIceCream)
  | Pudding -> List.init 15 (fun _ -> Pudding)
  | Fruit _ ->
      from_n_choose_2 [ Watermelon; Orange; Pineapple ]
      |> List.map (fun (fruit1, fruit2) ->
             List.init
               (if fruit1 = fruit2 then 2 else 3)
               (fun _ -> Fruit [ fruit1; fruit2 ]))
      |> List.flatten

(** Call card_to_deck (Appetizer z) will return a list of 3 elements 'Appetizer z' *)
let card_to_deck = function
  | Nigiri n -> List.init (nigiri_distribution n) (fun _ -> Nigiri n)
  | SushiRoll sr -> match_sushi_roll sr
  | Appetizer a -> match_appetizer a
  | Special s -> List.init 3 (fun index -> Special (match_special s index))
  | Dessert d -> match_dessert d |> List.map (fun dessert -> Dessert dessert)
  | FaceDown _ -> []

(** Create a [deck] from a [menu] adding [nigiri] cards*)
let create_deck menu =
  let sr, a1, a2, a3, s1, s2, d = menu in
  if not (is_well_built menu) then
    raise (Invalid_argument "Appetizer or Special are not unique")
  else
    (* Match each and every [card] to a [card list] of this type of food, then concatenate the list to a unique [deck] *)
    let deck_template =
      [
        Nigiri Egg;
        Nigiri Salmon;
        Nigiri Squid;
        SushiRoll sr;
        Appetizer a1;
        Appetizer a2;
        Appetizer a3;
        Special s1;
        Special s2;
      ]
      |> List.map card_to_deck |> List.flatten
    in
    (deck_template, d)

(** Filter all the desserts in a [deck] *)
let filter_desserts (deck, dessert_type) =
  ( List.filter
      (fun card -> match card with Dessert _ -> true | _ -> false)
      deck,
    dessert_type )

let create_deck_keeping_desserts (deck : deck) (menu : menu) : deck =
  let complete_deck, dessert_type = create_deck menu in
  (complete_deck @ (filter_desserts deck |> fst), dessert_type)

(** Associate a random bits to every element of the list, than sort it by the bits, and remove them *)
let shuffle_cards cards = Utils.list_shuffle cards

let how_many_desserts nb_players round =
  if nb_players < 6 then
    match round with
    | 1 -> 5
    | 2 -> 3
    | 3 -> 2
    | _ -> raise (Invalid_argument "Round not supported")
  else 7 - (2 * (round - 1))

let add_desserts_to_deck (cards, dessert) nb_players round =
  List.init (how_many_desserts nb_players round) (fun _ -> Dessert dessert)
  @ cards

(** [get_n_cards] : return n cards from a [card list]. If there are not enough cards, raise an error *)
let get_n_cards cards n =
  let rec gnc cards hand n =
    if n = 0 then (hand, cards)
    else
      match cards with
      | [] -> raise (Invalid_argument "Not enough cards in deck to pick")
      | card :: cards -> gnc cards (card :: hand) (n - 1)
  in
  gnc cards [] n

(** Return the number of cards to deal per player for each round *)
let number_of_cards_to_deal ~nb_players = 11 - (nb_players / 2)

(** [deal_cards] : Deal a certain amount of cards based on the number of players and the round they are in. Returns all hands and a deck of undistributed cards *)
let deal_cards deck ~nb_players ~round =
  let _, dessert = deck in
  let total_of_card_per_player = number_of_cards_to_deal ~nb_players in
  let cards = add_desserts_to_deck deck nb_players round |> shuffle_cards in
  let hands, deck =
    List.fold_left
      (fun (hands, cards) _ ->
        let new_hand, cards = get_n_cards cards total_of_card_per_player in
        (new_hand :: hands, cards))
      ([], cards)
      (List.init nb_players (fun _ -> []))
  in
  (hands, (deck, dessert))

(** Using a module to avoid conflicts with the [card] type. *)
module CardType = struct
  type t =
    | Nigiri
    | Maki
    | Uramaki
    | Temaki
    | Dumpling
    | Edamame
    | Eel
    | Onigiri
    | MisoSoup
    | Sashimi
    | Tempura
    | Tofu
    | Chopsticks
    | Spoon
    | Menu
    | SoySauce
    | SpecialOrder
    | TakeOutBox
    | Tea
    | Wasabi
    | MatchaIceCream
    | Fruit
    | Pudding

  (** [card_type_of_card card] is the type of [card]. *)
  let rec card_type_of_card : card -> t = function
    | Nigiri _ -> Nigiri
    | SushiRoll (Maki _) -> Maki
    | SushiRoll (Uramaki _) -> Uramaki
    | SushiRoll Temaki -> Temaki
    | Appetizer Dumpling -> Dumpling
    | Appetizer Edamame -> Edamame
    | Appetizer Eel -> Eel
    | Appetizer (Onigiri _) -> Onigiri
    | Appetizer MisoSoup -> MisoSoup
    | Appetizer Sashimi -> Sashimi
    | Appetizer Tempura -> Tempura
    | Appetizer Tofu -> Tofu
    | Special (Chopsticks _) -> Chopsticks
    | Special (Spoon _) -> Spoon
    | Special (Menu _) -> Menu
    | Special SoySauce -> SoySauce
    | Special SpecialOrder -> SpecialOrder
    | Special (TakeOutBox _) -> TakeOutBox
    | Special Tea -> Tea
    | Special (Wasabi _) -> Wasabi
    | Dessert MatchaIceCream -> MatchaIceCream
    | Dessert (Fruit _) -> Fruit
    | Dessert Pudding -> Pudding
    | FaceDown card -> card_type_of_card card
end

(** Remove from a [deck] the n firsts cards with type matching the card passed in argument *)
let remove_n_cards_of_type (cards, dessert) ~to_remove card =
  if to_remove = 0 then (cards, dessert)
  else
    ( List.fold_left
        (fun (n, acc) c ->
          if CardType.card_type_of_card c = CardType.card_type_of_card card then
            (n - 1, acc)
          else (n, c :: acc))
        (to_remove, []) cards
      |> snd,
      dessert )
