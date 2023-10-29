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
