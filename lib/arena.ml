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
[@@deriving eq]

type game_ending = {
  winners : win;  (** Winner/s. *)
  players : (string * int) list;  (** List of players sorted by score. *)
}
(** Game ending. It contains the winners and the players sorted by score. *)

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
    public counterpart is given by [game_status]. It is easy
    to convert an internal game status to a game status, but not the other way around
    See [game_status_of_internal_game_status] for more details.
    *)

(** Quick conversion from [internal_game_status] to [game_status]. Should
    be used to give players information about the game. *)
let game_status_of_internal_game_status internal_game_status =
  let players =
    List.map (fun { player; _ } -> player) internal_game_status.players
  in
  {
    players;
    current_round = internal_game_status.current_round;
    current_turn = internal_game_status.current_turn;
    menu = internal_game_status.menu;
  }

let player_status_of_strategized_player (strat_player : strategized_player) =
  { player = strat_player.player; hand = strat_player.hand }

(** Quick conversion from [internal_game_status] to [player_status]. Should
    be used to give players information about the game. *)

let initial_turn_status =
  {
    played_cards = [];
    played_specials = [];
    played_miso_soups = [];
    played_uramakis = [];
  }

(** Initializes an [internal_game_status] from [game_settings]. 
    @raise Invalid_argument if there are not enough or too many players. *)
let initial_game_status (game_settings : game_settings) : internal_game_status =
  let players =
    List.mapi
      (fun i (strategy, name) ->
        let player = default_named_player name in
        let player = { player with id = i } in
        { player; strategy; hand = [] })
      game_settings.players
  in
  let menu = game_settings.menu in
  let _, _, _, _, s1, s2, _ = menu in
  if List.length players < 2 then raise (Invalid_argument "Not enough players")
  else if List.length players > 8 then
    raise (Invalid_argument "Too many players")
  else if
    List.length players >= 7
    && (CardType.card_type_of_card (Special s1) = CardType.Menu
       || CardType.card_type_of_card (Special s2) = CardType.Menu)
  then
    raise
      (Invalid_argument
         "Invalid menu : Menu should not be selected with 7 or 8 players")
  else
    {
      players;
      played_uramakis = 0;
      total_special_order_copying_desserts = 0;
      current_round = 1;
      current_turn = 1;
      menu;
      deck = create_deck menu;
      turn_status = initial_turn_status;
    }

let find_splayer_with_id id igs =
  List.find (fun sp -> sp.player.id = id) igs.players

(** Find a player with a certain id in a list of players 
   @raise Not_found if no players have that id *)
let find_player_with_id id igs =
  find_splayer_with_id id igs |> fun sp -> sp.player

(** Map a function on the players of an [internal_game_status]. *)
let map_sps f igs = { igs with players = f igs.players }

let map_turn_status f igs = { igs with turn_status = f igs.turn_status }

let sort_card player_id card sps =
  let f ts =
    match CardType.card_type_of_card card with
    | Menu | TakeOutBox ->
        {
          ts with
          played_specials = (player_id, card) :: sps.turn_status.played_specials;
        }
    | MisoSoup ->
        {
          ts with
          played_miso_soups =
            (player_id, card) :: sps.turn_status.played_miso_soups;
        }
    | Uramaki ->
        {
          ts with
          played_uramakis =
            (List.assoc_opt player_id ts.played_uramakis |> function
             | None -> (player_id, [ card ]) :: ts.played_uramakis
             | Some played_cards ->
                 (player_id, card :: played_cards)
                 :: List.remove_assoc player_id ts.played_uramakis);
        }
    | _ ->
        {
          ts with
          played_cards = (player_id, card) :: sps.turn_status.played_cards;
        }
  in
  map_turn_status f sps

let remove_card_from_hand player_id card igs =
  let f =
    List.map (fun sp ->
        if sp.player.id = player_id then
          {
            sp with
            hand =
              remove_n_cards_of_type ~strict:true sp.hand ~total_to_remove:1
                card;
          }
        else sp)
  in
  map_sps f igs

let add_card_to_table player_id card igs =
  let f =
    List.map (fun sp ->
        if sp.player.id = player_id then
          {
            sp with
            player = { sp.player with table = card :: sp.player.table };
          }
        else sp)
  in
  map_sps f igs

let add_card_to_hand player_id card igs =
  let f =
    List.map (fun sp ->
        if sp.player.id = player_id then { sp with hand = card :: sp.hand }
        else sp)
  in
  map_sps f igs

let remove_card_from_table player_id card igs =
  let f =
    List.map (fun sp ->
        if sp.player.id = player_id then
          {
            sp with
            player =
              {
                sp.player with
                table =
                  remove_n_cards_of_type ~strict:true sp.player.table
                    ~total_to_remove:1 card;
              };
          }
        else sp)
  in
  map_sps f igs

let validate_card_in_table splayer card =
  if List.mem card splayer.player.table then card else raise Invalid_choice

let validate_card_in_hand player_id igs card =
  let sp = find_splayer_with_id player_id igs in
  if List.mem card sp.hand then card else raise Invalid_choice

let get_card_to_copy player_id igs =
  let gs = game_status_of_internal_game_status igs in
  let player = find_splayer_with_id player_id igs in
  player.strategy.choose_card_to_copy gs player.player
  |> validate_card_in_table player

let remove_card_from_played_cards player_id card igs =
  let f ts =
    {
      ts with
      played_cards =
        List.filter
          (fun (id, c) -> id <> player_id || c <> card)
          ts.played_cards;
    }
  in
  map_turn_status f igs

let remove_card_from_specials player_id card igs =
  let f ts =
    {
      ts with
      played_specials =
        List.filter
          (fun (id, c) -> id <> player_id || c <> card)
          ts.played_specials;
    }
  in
  map_turn_status f igs

let play_card player_id card igs =
  (match card with
  | Nigiri card -> (
      let player = find_player_with_id player_id igs in
      List.find_opt
        (function Special (Wasabi None) -> true | _ -> false)
        player.table
      |> function
      | None ->
          remove_card_from_hand player_id (Nigiri card) igs
          |> add_card_to_table player_id (Nigiri card)
      | Some wasabi_card ->
          remove_card_from_hand player_id (Nigiri card) igs
          |> remove_card_from_table player_id wasabi_card
          |> add_card_to_table player_id (Special (Wasabi (Some card))))
  | card ->
      igs
      |> remove_card_from_hand player_id card
      |> add_card_to_table player_id card)
  |> remove_card_from_played_cards player_id card

let play_chopsticks player_id chopsticks_card igs =
  let splayer = find_splayer_with_id player_id igs in
  let gs = game_status_of_internal_game_status igs in
  let ps = player_status_of_strategized_player splayer in
  splayer.strategy.play_chopsticks gs ps |> function
  | None -> igs
  | Some card ->
      let card = validate_card_in_hand player_id igs card in
      igs |> sort_card player_id card
      |> remove_card_from_table player_id chopsticks_card
      |> add_card_to_hand player_id chopsticks_card

let validate_card_in_card_list card_list card =
  if List.mem card card_list then card else raise Invalid_choice

let play_menu player_id menu_card igs =
  let splayer = find_splayer_with_id player_id igs in
  let gs = game_status_of_internal_game_status igs in
  let options, deck = get_n_cards_from_deck igs.deck 4 in
  if List.length options < 4 then
    igs |> remove_card_from_hand player_id menu_card
  else
    let card =
      splayer.strategy.choose_card_from_deck gs splayer.player ~options
      |> validate_card_in_card_list options
    in
    { igs with deck }
    |> add_card_to_hand player_id card
    |> sort_card player_id card
    |> remove_card_from_hand player_id menu_card

let use_spoon player1 player2 ~spoon_card card igs =
  igs
  |> remove_card_from_table player1 spoon_card
  |> remove_card_from_hand player2 card
  |> add_card_to_hand player1 card
  |> sort_card player1 card
  |> add_card_to_hand player2 spoon_card

(** Returns the list of player after the player with id [player_id] concatenated 
    * with the list of players before the player with id [player_id] *)
let player_list_spoon player_id igs =
  let rec aux acc = function
    | [] -> acc
    | sp :: sps ->
        if sp.player.id = player_id then sps @ acc else aux (sp :: acc) sps
  in
  aux [] igs.players

let play_spoon player_id spoon_card igs =
  let splayer = find_splayer_with_id player_id igs in
  splayer.strategy.play_spoon
    (game_status_of_internal_game_status igs)
    (player_status_of_strategized_player splayer)
  |> function
  | None -> igs
  | Some spoon_choice -> (
      List.fold_left
        (fun acc sp ->
          match acc with
          | Some _ -> acc
          | None ->
              let filter card =
                match spoon_choice with
                | Generic card_type ->
                    CardType.card_type_of_card card = card_type
                | Specific chosen_card -> chosen_card = card
              in
              let cards = List.filter filter sp.hand in
              if Utils.is_empty cards then None
              else if Utils.partition_list ( = ) cards |> List.length = 1 then
                Some (sp.player.id, List.hd cards)
              else
                let card =
                  sp.strategy.choose_card_to_give
                    (game_status_of_internal_game_status igs)
                    sp.player ~options:cards
                  |> validate_card_in_hand sp.player.id igs
                in
                Some (sp.player.id, card))
        None
        (player_list_spoon player_id igs)
      |> function
      | None -> igs
      | Some (player_id2, card) ->
          use_spoon player_id player_id2 card ~spoon_card igs)

let flip_down_cards player_id cards igs =
  let f =
    List.map (fun sp ->
        if sp.player.id = player_id then
          {
            sp with
            player =
              {
                sp.player with
                table =
                  List.fold_left
                    (fun (remaining, acc) card ->
                      if List.mem card remaining then
                        ( remove_n_cards_of_type ~strict:true remaining
                            ~total_to_remove:1 card,
                          FaceDown card :: acc )
                      else (remaining, card :: acc))
                    (cards, []) sp.player.table
                  |> snd;
              };
          }
        else sp)
  in

  map_sps f igs

let validate_sublist_in_table table cards =
  if Utils.includes table cards then cards else raise Invalid_choice

let play_take_out_box player_id card igs =
  let splayer = find_splayer_with_id player_id igs in
  let gs = game_status_of_internal_game_status igs in
  let cards =
    splayer.strategy.choose_cards_to_flip gs splayer.player
    |> validate_sublist_in_table splayer.player.table
  in
  igs |> flip_down_cards player_id cards |> remove_card_from_hand player_id card

let play_normal_cards igs =
  List.fold_left
    (fun igs (player_id, card) ->
      match CardType.card_type_of_card card with
      | SpecialOrder ->
          let player = find_player_with_id player_id igs in
          if Utils.is_empty player.table then play_card player_id card igs
          else
            let card_to_copy = get_card_to_copy player_id igs in
            (match card_to_copy with
            | Dessert _ ->
                {
                  igs with
                  total_special_order_copying_desserts =
                    igs.total_special_order_copying_desserts + 1;
                }
            | _ -> igs)
            |> add_card_to_hand player_id card_to_copy
            |> sort_card player_id card_to_copy
            |> remove_card_from_hand player_id card
            |> remove_card_from_played_cards player_id card
      | _ -> play_card player_id card igs)
    igs igs.turn_status.played_cards

(** Extract priority from a card that has one 
   @raises Invalid_argument when card has no priority *)
let get_priority_from_card = function
  | Special (Chopsticks p)
  | Special (Spoon p)
  | Special (Menu p)
  | Special (TakeOutBox p) ->
      p
  | _ -> raise (Invalid_argument "Card has no priority")

let compare_specials sp1 sp2 =
  let p1 = get_priority_from_card (snd sp1) in
  let p2 = get_priority_from_card (snd sp2) in
  compare p1 p2

let play_special_card (player_id, card) igs =
  let igs =
    map_turn_status
      (fun ts ->
        {
          ts with
          played_specials = List.sort compare_specials ts.played_specials;
        })
      igs
  in
  (match CardType.card_type_of_card card with
  | Chopsticks -> play_chopsticks player_id card igs
  | Spoon -> play_spoon player_id card igs
  | Menu -> play_menu player_id card igs
  | TakeOutBox -> play_take_out_box player_id card igs
  | _ -> raise (Failure "This should not happen, an invalid card was played"))
  |> remove_card_from_specials player_id card

let play_special_cards igs =
  List.fold_left
    (fun igs x -> play_special_card x igs)
    igs igs.turn_status.played_specials

let play_miso_soups igs =
  if igs.turn_status.played_miso_soups |> List.length = 1 then
    let player_id, card = List.hd igs.turn_status.played_miso_soups in
    play_card player_id card igs
  else
    List.fold_left
      (fun igs (player_id, card) -> remove_card_from_hand player_id card igs)
      igs igs.turn_status.played_miso_soups

(** Returns the total uramakis icons, both in cards and already on table. *)
let count_uramakis player cards =
  let sum_uramakis acc = function
    | SushiRoll (Uramaki n) -> n + acc
    | _ -> acc
  in
  let uramaki_icons = List.fold_left sum_uramakis 0 cards in
  List.fold_left sum_uramakis uramaki_icons player.table

let keep_countable_uramakis igs =
  let players_and_icons =
    List.map
      (fun (player_id, cards) ->
        ( player_id,
          cards,
          count_uramakis (find_player_with_id player_id igs) cards ))
      igs.turn_status.played_uramakis
  in
  List.fold_left
    (fun (players_to_remove_uramakis, igs) (player_id, cards, icons) ->
      if icons >= 10 then
        ((player_id, cards, icons) :: players_to_remove_uramakis, igs)
      else
        ( players_to_remove_uramakis,
          List.fold_left
            (fun igs card ->
              igs
              |> remove_card_from_hand player_id card
              |> add_card_to_table player_id card)
            igs cards ))
    ([], igs) players_and_icons

let add_points_to_player points player =
  { player with score = player.score + points }

let update_player_table f player = { player with table = f player.table }

let update_points_and_remove_uramakis igs =
  List.fold_left
    (fun igs (player_id, cards, points) ->
      let player =
        find_player_with_id player_id igs
        |> add_points_to_player points
        |> update_player_table
             (List.filter (fun c -> CardType.card_type_of_card c <> Uramaki))
      in
      let players =
        List.map
          (fun p -> if p.player.id = player_id then { p with player } else p)
          igs.players
      in
      List.fold_left
        (fun igs card -> remove_card_from_hand player_id card igs)
        { igs with players } cards)
    igs

let play_uramakis igs =
  let players_to_remove_uramakis, igs = keep_countable_uramakis igs in
  List.map (fun (_, _, icons) -> icons) players_to_remove_uramakis
  |> Points.get_positions
  |> Points.uramaki_points ~during_round:true igs.played_uramakis
  |> List.map2
       (fun (player_id, cards, _) (_, points) -> (player_id, cards, points))
       players_to_remove_uramakis
  |> update_points_and_remove_uramakis igs
  |> map_turn_status (fun ts -> { ts with played_uramakis = [] })
  |> fun igs ->
  {
    igs with
    played_uramakis =
      igs.played_uramakis + List.length players_to_remove_uramakis;
  }

let play_all_cards igs =
  let rec aux igs =
    if
      Utils.is_empty igs.turn_status.played_cards
      && Utils.is_empty igs.turn_status.played_specials
    then igs
    else igs |> play_normal_cards |> play_special_cards |> aux
  in
  aux igs |> play_miso_soups |> play_uramakis

(** All players give their hand to the player on their left. *)
let pass_hands sps =
  if Utils.is_empty sps then sps
  else
    let last_hand, sps =
      List.fold_left_map
        (fun hand sp ->
          let hand_to_pass = sp.hand in
          (hand_to_pass, { sp with hand }))
        (List.hd sps).hand sps
    in
    match sps with [] -> [] | sp :: sps -> { sp with hand = last_hand } :: sps

(** Advance one turn. Nothing needs to be reset *)
let advance_one_turn (internal_game_status : internal_game_status) :
    internal_game_status =
  {
    internal_game_status with
    current_turn = internal_game_status.current_turn + 1;
    turn_status = initial_turn_status;
  }

let get_cards_to_play igs =
  List.fold_left
    (fun igs splayer ->
      let gs = game_status_of_internal_game_status igs in
      let ps = player_status_of_strategized_player splayer in
      let card =
        splayer.strategy.choose_card gs ps
        |> validate_card_in_hand splayer.player.id igs
      in
      sort_card splayer.player.id card igs)
    igs igs.players

let add_spoon_and_chopsticks_to_play igs =
  let playing_spoons =
    List.fold_left
      (fun spoons splayer ->
        List.fold_left
          (fun spoons card ->
            if
              CardType.card_type_of_card card = Spoon
              || CardType.card_type_of_card card = Chopsticks
            then (splayer.player.id, card) :: spoons
            else spoons)
          spoons splayer.player.table)
      [] igs.players
  in
  {
    igs with
    turn_status =
      {
        igs.turn_status with
        played_specials = playing_spoons @ igs.turn_status.played_specials;
      };
  }

(** Play a turn of Sushi Go Party. *)
let play_turn (internal_game_status : internal_game_status) =
  internal_game_status |> add_spoon_and_chopsticks_to_play |> get_cards_to_play
  |> play_all_cards |> map_sps pass_hands

(** Advance one round. They current turn is reset to 1. *)
let advance_one_round (internal_game_status : internal_game_status) :
    internal_game_status =
  {
    internal_game_status with
    players =
      List.map
        (fun p ->
          {
            p with
            hand = [];
            player =
              {
                p.player with
                desserts =
                  p.player.desserts
                  @ List.filter_map
                      (function Dessert d -> Some d | _ -> None)
                      p.player.table;
                table = [];
              };
          })
        internal_game_status.players;
    current_round = internal_game_status.current_round + 1;
    current_turn = 1;
    played_uramakis = 0;
    deck =
      create_deck_keeping_desserts internal_game_status.deck
        internal_game_status.menu;
  }

(** Extract [player list] from [strategized_player] *)
let extract_strategized_players (players : strategized_player list) =
  List.fold_left
    (fun players strat_player -> strat_player.player :: players)
    [] players

(** Remove player with a certain id in a list of players *)
let remove_player_with_id players id =
  List.filter (fun player -> player.id <> id) players

(** Reforms [strategized_player list] from [player list] and [strategized_player list] 
    @raise Invalid_argument when list have not the same length.
    @raise Not_found when there is [player] who is not is the [strategized_player list]
    *)
let zip_to_strategized_players (players : player list)
    (strategized_players : strategized_player list) : strategized_player list =
  let find_player_with_id players id =
    List.find (fun player -> player.id = id) players
  in
  if List.length players <> List.length strategized_players then
    raise (Invalid_argument "List should have the same length")
  else
    List.fold_left_map
      (fun players strat_player ->
        ( remove_player_with_id players strat_player.player.id,
          {
            strat_player with
            player = find_player_with_id players strat_player.player.id;
          } ))
      players strategized_players
    |> snd

(** Compute points for each [player] by 
   * Extracting players from [internal_game_status]
   * Updating scores
   * Zipping players with strategies 
 *)
let compute_points_round (internal_game_status : internal_game_status) :
    internal_game_status =
  let players = internal_game_status.players |> extract_strategized_players in
  let players =
    Points.count_round_points
      ~played_uramakis:internal_game_status.played_uramakis players
  in
  {
    internal_game_status with
    players = zip_to_strategized_players players internal_game_status.players;
  }

(** Deal cards at the start of a round *)
let deal_cards_at_start (internal_game_status : internal_game_status) =
  let deck_to_deal =
    remove_n_cards_of_type_from_deck internal_game_status.deck
      ~total_to_remove:internal_game_status.total_special_order_copying_desserts
      (Special SpecialOrder)
  in
  let hands, deck =
    deal_cards deck_to_deal
      ~nb_players:(List.length internal_game_status.players)
      ~round:internal_game_status.current_round
  in
  let players =
    List.map2
      (fun player hand -> { player with hand })
      internal_game_status.players hands
  in
  { internal_game_status with players; deck }

(** Play a round of Sushi Go Party. *)
let play_round (internal_game_status : internal_game_status) :
    internal_game_status =
  let number_of_turns =
    number_of_cards_to_deal
      ~nb_players:(List.length internal_game_status.players)
  in
  List.init number_of_turns (fun _ -> ())
  |> List.fold_left
       (fun acc _ -> acc |> play_turn |> advance_one_turn)
       (deal_cards_at_start internal_game_status)
  |> compute_points_round |> advance_one_round

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
  List.init 3 (fun _ -> ())
  |> List.fold_left (fun acc _ -> acc |> play_round) internal_game_status
  |> (fun x -> x.players)
  |> List.map (fun { player; _ } -> player)
  |> Points.count_dessert_points |> game_ending_of_player_list
