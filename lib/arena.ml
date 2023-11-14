open Cards
open Player
open Points

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
[@@deriving eq]

type game_ending = {
  winners : win;  (** Winner/s. *)
  players : (string * int) list;  (** List of players sorted by score. *)
}
(** Game ending. It contains the winners and the players sorted by score. *)

type game_settings = { players : (player_strategy * string) list; menu : menu }
(** Basic game settings. It contains the players (how they are called and their strategy) and the menu. *)

type strategized_player = { player : player; strategy : player_strategy }
(** Player with a strategy. *)

type internal_game_status = {
  players : strategized_player list;
  played_uramakis : int;
  total_special_order_copying_desserts : int;
  current_round : int;
  current_turn : int;
  deck : deck;
  menu : menu;
}
(** Current game status. This contains the details needed to
    compute the next game status. This information is private and a
    public counterpart is given by [game_status]. It is easy
    to convert an internal game status to a game status, but not the other way around
    See [game_status_of_internal_game_status] for more details.
    *)

(** Quick conversion from [internal_game_status] to [game_status]. Should
    be used to give players information about the game. *)
let[@warning "-32"] game_status_of_internal_game_status internal_game_status =
  let players =
    List.map (fun { player; _ } -> player) internal_game_status.players
  in
  {
    players;
    current_round = internal_game_status.current_round;
    current_turn = internal_game_status.current_turn;
    menu = internal_game_status.menu;
  }

(** Initializes an [internal_game_status] from [game_settings]. 
    @raise Invalid_argument if there are not enough or too many players. *)
let initial_game_status (game_settings : game_settings) : internal_game_status =
  let players =
    List.mapi
      (fun i (strategy, name) ->
        let player = default_named_player name in
        let player = { player with id = i } in
        { player; strategy })
      game_settings.players
  in
  let menu = game_settings.menu in
  if List.length players < 2 then raise (Invalid_argument "Not enough players")
  else if List.length players > 8 then
    raise (Invalid_argument "Too many players")
  else
    {
      players;
      played_uramakis = 0;
      total_special_order_copying_desserts = 0;
      current_round = 1;
      current_turn = 1;
      menu;
      deck = create_deck menu;
    }

(** Advance one turn. Nothing needs to be reset *)
let advance_one_turn (internal_game_status : internal_game_status) :
    internal_game_status =
  {
    internal_game_status with
    current_turn = internal_game_status.current_turn + 1;
  }

(** Play a turn of Sushi Go Party. *)
let play_turn (internal_game_status : internal_game_status) :
    internal_game_status =
  internal_game_status |> advance_one_turn

(** Advance one round. They current turn is reset to 1. *)
let advance_one_round (internal_game_status : internal_game_status) :
    internal_game_status =
  {
    internal_game_status with
    current_round = internal_game_status.current_round + 1;
    current_turn = 1;
    played_uramakis = 0;
    deck =
      create_deck_keeping_desserts internal_game_status.deck
        internal_game_status.menu;
  }

(** Split [strategized_player list] into ([player list], [player_strategy list]) *)
let extract_strategized_players (players : strategized_player list) =
  List.fold_left
    (fun (p, s) sp -> (sp.player :: p, sp.strategy :: s))
    ([], []) players

(** Forms [strategized_player list] from [player list] and [player_strategy list] 
    @raise an Invalid_argument when list have not the same length.
    *)
let zip_to_strategized_players (players : player list)
    (player_strategies : player_strategy list) =
  let rec zip zipped p s =
    match (p, s) with
    | [], [] -> zipped
    | p :: ps, s :: ss -> zip ({ player = p; strategy = s } :: zipped) ps ss
    | _ -> raise (Invalid_argument "Each player should have a strategy")
  in
  zip [] players player_strategies

(** Compute points for each [player] by 
   * Extracting players from [internal_game_status]
   * Updating scores
   * Zipping players with strategies 
 *)
let compute_points_round (internal_game_status : internal_game_status) :
    internal_game_status =
  let players, strategies =
    internal_game_status.players |> extract_strategized_players
  in
  let players =
    count_round_points ~played_uramakis:internal_game_status.played_uramakis
      players
  in
  {
    internal_game_status with
    players = zip_to_strategized_players players strategies;
  }

(** Deal cards at the start of a round *)
let deal_cards_at_start (internal_game_status : internal_game_status) =
  let players, strategies =
    internal_game_status.players |> extract_strategized_players
  in
  let deck_to_deal =
    remove_n_cards_of_type internal_game_status.deck
      ~to_remove:internal_game_status.total_special_order_copying_desserts
      (Special SpecialOrder)
  in
  let hands, deck =
    deal_cards deck_to_deal
      ~nb_players:(List.length internal_game_status.players)
      ~round:internal_game_status.current_round
  in
  let players =
    List.map2 (fun player hand -> { player with table = hand }) players hands
  in
  {
    internal_game_status with
    players = zip_to_strategized_players players strategies;
    deck;
  }

(** Play a round of Sushi Go Party. *)
let play_round (internal_game_status : internal_game_status) :
    internal_game_status =
  let number_of_turns =
    number_of_cards_to_deal
      ~nb_players:(List.length internal_game_status.players)
  in
  let rec round_loop (internal_game_status : internal_game_status) =
    if internal_game_status.current_turn > number_of_turns then
      internal_game_status
    else internal_game_status |> play_turn |> round_loop
  in
  deal_cards_at_start internal_game_status
  |> round_loop |> compute_points_round |> advance_one_round

(** Compares the scores of two players. If they have the same score, we compare the number of desserts they have. *)
let compare_players_score p1 p2 =
  match compare p2.score p1.score with
  | 0 -> compare (List.length p2.desserts) (List.length p1.desserts)
  | x -> x

let decompose_player player = (player.name, player.score)

(** Computes the points for each player and returns a [game_ending] with the winners and the players sorted by score. *)
let game_ending_of_player_list players =
  let players = List.sort compare_players_score players in
  let first_player = List.hd players in
  let winners =
    List.filter (fun p -> compare p first_player = 0) players
    |> List.map decompose_player
  in
  let win =
    if List.length winners = 1 then Single (List.hd winners) else Draw winners
  in
  { winners = win; players = List.map decompose_player players }

(** From a  given [game_settings], this function plays a game of Sushi Go Party. *)
let arena (game_settings : game_settings) =
  let internal_game_status = initial_game_status game_settings in
  let rec loop (internal_game_status : internal_game_status) =
    if internal_game_status.current_round > 3 then internal_game_status
    else
      let internal_game_status = play_round internal_game_status in
      loop internal_game_status
  in
  let post_rounds_internal_game_status = loop internal_game_status in
  post_rounds_internal_game_status.players
  |> List.map (fun { player; _ } -> player)
  |> count_dessert_points |> game_ending_of_player_list
