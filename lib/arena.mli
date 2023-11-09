open Cards
open Player

type game_status = {
  players : player list;
  current_round : int;
  current_turn : int;
  menu : menu;
}
(** Current game status. This information is public, no details about the
players' hands are given. *)

type player_status = { player : player; hand : card list }
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

type choose_card_from_deck = game_status -> player_status -> card
(** When playing the [Menu] card, choose which card you want to take from the [deck]. *)

type choose_card_to_copy = game_status -> player_status -> card
(** When playing the [SpecialOrder] card, choose which card you want to copy. *)

type choose_cards_to_flip = game_status -> player_status -> card list
(** When playing the [TakeOutBox] card, choose which cards you want to put face down. *)

type choose_card_to_give = game_status -> player_status -> card
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

type game_settings = { players : (player_strategy * string) list; menu : menu }
(** Basic game settings. It contains the players (how they are called and their strategy) and the menu. *)

val arena : game_settings -> unit
(** Start a game with the given settings. *)
