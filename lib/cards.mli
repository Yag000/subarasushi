type nigiri = Egg | Salmon | Squid

val equal_nigiri : nigiri -> nigiri -> bool

type sushi_roll = Maki of int | Temaki | Uramaki of int

val equal_sushi_roll : sushi_roll -> sushi_roll -> bool

type shape = Circle | Triangle | Square | Rectangle

val equal_shape : shape -> shape -> bool

type fruit_kind = Watermelon | Orange | Pineapple

val equal_fruit_kind : fruit_kind -> fruit_kind -> bool

type appetizer =
  | Dumpling
  | Edamame
  | Eel
  | Onigiri of shape
  | MisoSoup
  | Sashimi
  | Tempura
  | Tofu

val equal_appetizer : appetizer -> appetizer -> bool

type special =
  | Chopsticks of int
  | Spoon of int
  | Menu of int
  | SoySauce
  | SpecialOrder
  | TakeOutBox of int
  | Tea
  | Wasabi of nigiri option

val equal_special : special -> special -> bool

type dessert = MatchaIceCream | Fruit of fruit_kind list | Pudding

val equal_dessert : dessert -> dessert -> bool

type card =
  | Nigiri of nigiri
  | SushiRoll of sushi_roll
  | Appetizer of appetizer
  | Special of special
  | Dessert of dessert
  | FaceDown of card

val equal_card : card -> card -> bool

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
(** Initializes a [menu] from a [default_menu]. *)

type deck
(** This type represents the complete game [deck] *)

val pp_deck : Format.formatter -> deck -> unit
val equal_deck : deck -> deck -> bool

type hand = card list

val equal_hand : hand -> hand -> bool

val create_deck : menu -> deck
(** [create_deck] : create a [deck] based on a certain [menu]. 
    Some categories of cards appear more often than others. *)

val create_deck_keeping_desserts : deck -> menu -> deck

val shuffle_cards : card list -> card list
(** [shuffle_cards] : shuffle a [card list] *)

val number_of_cards_to_deal : nb_players:int -> int
(** Return the number of cards to deal per player for each round *)

val deal_cards : deck -> nb_players:int -> round:int -> hand list * deck
(** [deal_cards] : from a [deck], the number of players, and the current round, distribute cards to each player and an undistributed [deck] of cards. *)

val remove_n_cards_of_type : deck -> to_remove:int -> card -> deck
(** Remove from a [deck] the n firsts cards with type matching the [card] passed in argument *)

(** Using a module to avoid conflicts with the [card] type. *)
module CardType : sig
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

  val card_type_of_card : card -> t
  (** [card_type_of_card card] is the type of [card]. *)
end
