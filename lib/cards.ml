type nigiri = Egg | Salmon | Squid
type sushi_roll = Maki of int | Temaki | Uramaki of int
type shape = Circle | Triangle | Square | Rectangle
type fruit_kind = Watermelon | Orange | Pineapple

type appetizer =
  | Dumpling
  | Edamame
  | Eel
  | Onigiri of shape
  | MisoSoup
  | Sashimi
  | Tempura
  | Tofu

type special =
  | Chopsticks of int
  | Spoon of int
  | Menu of int
  | SoySauce
  | SpecialOrder
  | TakeOutBox of int
  | Tea
  | Wasabi of nigiri option

type dessert = MatchaIceCream | Fruit of fruit_kind list | Pudding

type card =
  | Nigiri of nigiri
  | SushiRoll of sushi_roll
  | Appetizer of appetizer
  | Special of special
  | Dessert of dessert
  | FaceDown of card

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

let menu_of_default_menu = function
  | MyFirstMeal ->
      (Maki 0, Tempura, Sashimi, MisoSoup, Wasabi None, Tea, MatchaIceCream)
  | SushiGo ->
      (Maki 0, Tempura, Sashimi, Dumpling, Chopsticks 0, Wasabi None, Fruit [])
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
