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
