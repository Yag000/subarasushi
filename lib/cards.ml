type nigiri = Egg | Salmon | Squid
type sushi_roll = Maki of int | Temaki | Uramaki of int

type appetizer =
  | Dumpling
  | Edamame
  | Eel
  | Onigiri
  | MisoSoup
  | Sashimi
  | Tempura
  | Tofu

type special =
  | Chopsticks
  | Spoon
  | Menu
  | SoySauce
  | SpecialOrder
  | TakeOutBox
  | Tea
  | Wasabi

type dessert = MatchaIceCream | Fruit | Pudding

type card =
  | Nigiri of nigiri
  | SushiRoll of sushi_roll
  | Appetizer of appetizer
  | Special of special
  | Dessert of dessert

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
      (Maki 0, Tempura, Sashimi, MisoSoup, Wasabi, Tea, MatchaIceCream)
  | SushiGo -> (Maki 0, Tempura, Sashimi, Dumpling, Chopsticks, Wasabi, Fruit)
  | PartySampler ->
      (Temaki, Tempura, Dumpling, Tofu, Wasabi, Menu, MatchaIceCream)
  | MasterMenu -> (Temaki, Onigiri, Tofu, Sashimi, Spoon, TakeOutBox, Fruit)
  | PointsPlatter ->
      (Uramaki 0, Onigiri, Dumpling, Edamame, SpecialOrder, Tea, MatchaIceCream)
  | CutthroatCombo -> (Temaki, Eel, Tofu, MisoSoup, Spoon, SoySauce, Pudding)
  | BigBanquet ->
      (Maki 0, Tempura, Dumpling, Eel, Spoon, Chopsticks, MatchaIceCream)
  | DinnerForTwo ->
      (Uramaki 0, Onigiri, Tofu, MisoSoup, Menu, SpecialOrder, Fruit)
