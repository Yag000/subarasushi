open Cards
open Player

exception Invalid_choice

type game_status = {
  players : player list;
  current_round : int;
  current_turn : int;
  menu : menu;
}
(** Current game status. This information is public, no details about the
players' hands are given. *)

type player_status = { player : player; hand : hand }
(** Player status. This information should only be given to the [player] that has to make a decision.
    The player's hand is given, but no other player's hand is given.
 *)

type choose_card_to_play = game_status -> player_status -> card
(** Choose the [card] that you want to play. *)

type play_chopsticks = game_status -> player_status -> card option
(** Choose if you want to play your [Chopsticks] and which card you want. [None] means you don't want to play chopsticks. *)

(** When playing the [Spoon] card, you can either give a specific [card] or a card of a [card_type]. 
    For example if you want a [Nigiri] card, without caring which one, you can give [Generic CardType.Nigiri], 
    but if you want an egg nigiri then you can give [Specific (Nigiri Egg)].
    *)
type spoon_choice = Specific of card | Generic of CardType.t

type play_spoon = game_status -> player_status -> spoon_choice option
(** Choose if you want to play your [Spoon] card. [None] means you don't want to play your [card]. *)

type choose_card_from_deck = game_status -> player -> options:card list -> card
(** When playing the [Menu] card, choose which card you want to take from the deck. *)

type choose_card_to_copy = game_status -> player -> card
(** When playing the [SpecialOrder] card, choose which card you want to copy. *)

type choose_cards_to_flip = game_status -> player -> card list
(** When playing the [TakeOutBox] card, choose which cards you want to put face down. *)

type choose_card_to_give = game_status -> player -> options:card list -> card
(** When someone plays the [Spoon] card, choose which card you want to give them.
    This is only called if you have to make a choice, for example if someone asks for a [Maki 3] card, 
    you have no choice, so this function is not called. But if someone asks for a [Maki] card, then you have a choice,
    so this function is called.
 *)

type player_strategy = {
  choose_card : choose_card_to_play;
  play_chopsticks : play_chopsticks;
  play_spoon : play_spoon;
  choose_card_from_deck : choose_card_from_deck;
  choose_card_to_copy : choose_card_to_copy;
  choose_cards_to_flip : choose_cards_to_flip;
  choose_card_to_give : choose_card_to_give;
}
(** Player strategy. *)

(** Win. It can be a single player or a draw between multiple players. *)
type win =
  | Single of (string * int)  (** Single player win *)
  | Draw of (string * int) list  (** Draw between multiple players *)

val equal_win : win -> win -> bool
(** Equality function for [win]. *)

type game_ending = {
  winners : win;  (** Winner/s. *)
  players : (string * int) list;  (** List of players sorted by score. *)
}
(** Game ending. It contains the winners and the players sorted by score. *)

val game_ending_of_player_list : player list -> game_ending
(** Create a game ending from a list of players. *)

type game_settings = { players : (player_strategy * string) list; menu : menu }
(** Basic game settings. It contains the players (how they are called and their strategy) and the menu. *)

type strategized_player = {
  player : player;
  hand : hand;
  strategy : player_strategy;
}
(** Player with a strategy. *)

type turn_status = {
  played_cards : (int * card) list;
  played_specials : (int * card) list;
  played_miso_soups : (int * card) list;
  played_uramakis : (int * card list) list;
}

val initial_turn_status : turn_status
(** Initial turn status. *)

type internal_game_status = {
  players : strategized_player list;
  played_uramakis : int;
  total_special_order_copying_desserts : int;
  current_round : int;
  current_turn : int;
  deck : deck;
  menu : menu;
  turn_status : turn_status;
}
(** Current game status. This contains the details needed to
    compute the next game status. This information is private and a
    public counterpart is given by [game_status]. It should only be 
    used for testing purposes. *)

val pass_hands : strategized_player list -> strategized_player list
(** All players give their hand to the player on their left. *)

val play_turn : internal_game_status -> internal_game_status
(** Play a turn. *)

val arena : game_settings -> game_ending
(** Start a game with the given settings. *)
