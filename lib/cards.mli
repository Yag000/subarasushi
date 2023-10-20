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

(** Set of default menus for the game. *)
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
(** This type represents the menu of a restaurant. It does not include the [nigiri] cards since they are always available.
There exists a set of default menus that are available to the player. The player can also create their own menu, creating an instance of this type. *)

val menu_of_default_menu : default_menu -> menu
(** Initializes a menu from a default menu. *)
