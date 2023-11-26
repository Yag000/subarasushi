open Cards
open Player

type game_status = {
  players : player list;
  current_round : int;
  current_turn : int;
  menu : menu;
  number_of_tries : int;
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
let game_status_of_internal_game_status internal_game_status ~number_of_tries =
  let players =
    List.map (fun { player; _ } -> player) internal_game_status.players
  in
  {
    players;
    current_round = internal_game_status.current_round;
    current_turn = internal_game_status.current_turn;
    menu = internal_game_status.menu;
    number_of_tries;
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
    }

(** Advance one turn. Nothing needs to be reset *)
let advance_one_turn (internal_game_status : internal_game_status) :
    internal_game_status =
  {
    internal_game_status with
    current_turn = internal_game_status.current_turn + 1;
  }

type turn_status = {
  deck : deck;
  played_uramakis : int;
  played_miso_soup : int;
  special_order_copying_desserts : int;
  playing_spoon : (int * card) list;
  playing_chopsticks : (int * card) list;
  classic_cards_to_place : (strategized_player * card) list;
  special_order_to_use : (strategized_player * card) list;
  action_cards_to_use : (strategized_player * card) list;
  miso_and_ura_to_place : (strategized_player * card) list;
}
(** This type is used to represent what's going on during a turn *)

(** A default turn status *)
let initial_turn_status (internal_game_status : internal_game_status) =
  {
    deck = internal_game_status.deck;
    played_uramakis = 0;
    played_miso_soup = 0;
    special_order_copying_desserts = 0;
    playing_spoon = [];
    playing_chopsticks = [];
    classic_cards_to_place = [];
    special_order_to_use = [];
    action_cards_to_use = [];
    miso_and_ura_to_place = [];
  }

(** Builds a [player_status] based on an [internal_game_status] *)
let build_player_status strategized_player =
  { player = strategized_player.player; hand = strategized_player.hand }

(** Insert an element into a sorted list so that for any elements a, b in 
    the list : f a <= f b. (b is situated after a in the list)*)
let rec insert (f : 'a -> int) (e : 'a) = function
  | [] -> [ e ]
  | x :: xs -> if f e <= f x then e :: x :: xs else x :: insert f e xs

let add_classic_card_to_queue (ts : turn_status) (p : strategized_player)
    (c : card) =
  { ts with classic_cards_to_place = (p, c) :: ts.classic_cards_to_place }

let add_special_order_to_queue (ts : turn_status) (p : strategized_player)
    (c : card) =
  { ts with special_order_to_use = (p, c) :: ts.special_order_to_use }

let add_action_card_to_queue f (ts : turn_status) (p : strategized_player)
    (c : card) =
  { ts with action_cards_to_use = insert f (p, c) ts.action_cards_to_use }

let add_miso_ura_cards_to_queue (ts : turn_status) (p : strategized_player)
    (c : card) =
  { ts with miso_and_ura_to_place = (p, c) :: ts.miso_and_ura_to_place }

(** Increments the uramakis counter when a player plays one *)
let increment_played_uramakis_counter (turn_status : turn_status) =
  { turn_status with played_uramakis = turn_status.played_uramakis + 1 }

(** Increments the [MisoSoup] counter when a player plays one *)
let increment_miso_soup_counter (turn_status : turn_status) =
  { turn_status with played_miso_soup = turn_status.played_miso_soup + 1 }

let increment_spec_ord_copying_desserts (turn_status : turn_status) =
  {
    turn_status with
    special_order_copying_desserts =
      turn_status.special_order_copying_desserts + 1;
  }

(** Given a [card list] c and a [CardType.t list] cts, filter all cards in c that have their 
 [CardType.t] the cts. *)
let search_card_list_for_card_types (c : card list) (cts : CardType.t list) =
  List.filter (function FaceDown _ -> false | _ -> true) c
  |> List.filter (fun card -> List.mem (CardType.card_type_of_card card) cts)

(** Assert a list is fully contained in one another *)
let assert_player_choices_are_in_bounds cards choices =
  List.length cards >= List.length choices && Utils.includes cards choices

(** Extract priority from a card that has one 
   @raises Invalid_argument when card has no priority *)
let get_priority_from_card = function
  | Special (Chopsticks p)
  | Special (Spoon p)
  | Special (Menu p)
  | Special (TakeOutBox p) ->
      p
  | _card -> raise (Invalid_argument "Card has no priority")

let action_sort_function x = snd x |> get_priority_from_card

let add_spoon_card_to_queue action_sort_function ts sp c =
  if
    List.exists
      (fun (id, card) -> id = sp.player.id && card = c)
      ts.playing_spoon
  then ts
  else add_action_card_to_queue action_sort_function ts sp c

let add_chopsticks_card_to_queue action_sort_function ts sp c =
  if
    List.exists
      (fun (id, card) -> id = sp.player.id && card = c)
      ts.playing_chopsticks
  then ts
  else add_action_card_to_queue action_sort_function ts sp c

(** Go through all the players and see if they have chopsticks or spoons in front of them. *)
let add_spoon_chopsticks_to_queue (sps : strategized_player list)
    (ts : turn_status) =
  List.fold_left
    (fun ts sp ->
      List.fold_left
        (fun ts c ->
          match c with
          | Special (Spoon _) ->
              add_spoon_card_to_queue action_sort_function ts sp c
          | Special (Chopsticks _) ->
              add_chopsticks_card_to_queue action_sort_function ts sp c
          | _ ->
              raise
                (Failure
                   "Unreachable, this list should only contain spoons and \
                    chopsticks"))
        ts
        (search_card_list_for_card_types sp.player.table
           [ CardType.Spoon; CardType.Chopsticks ]))
    ts sps

(** Searches for empty [Wasabi] card and add [Nigiri] to it *)
let add_nigiri_to_wasabi nigiri =
  let rec add_nigiri table = function
    | [] -> Nigiri nigiri :: table
    | Special (Wasabi None) :: cards ->
        (Special (Wasabi (Some nigiri)) :: table) @ cards
    | card :: t -> add_nigiri (card :: table) t
  in
  add_nigiri []

(** Add a card to a player's table *)
let put_card_on_table card table =
  match card with
  | Nigiri nigiri -> add_nigiri_to_wasabi nigiri table
  | card -> card :: table

(** List.map over [strategized_player list]. *)
let map_strategized_players (strategized_players : strategized_player list) f =
  List.map f strategized_players

(** Remove exactly one occurence of the exact [card] in a [card list] *)
let remove_card_from_cards_list (card : card) (cards : card list) =
  remove_n_cards_of_type ~strict:true cards ~total_to_remove:1 card

(** Update [strategized_player] only if their id equal the given id *)
let update_strategized_player_with_id (id : int)
    (new_strategized_player : strategized_player -> strategized_player)
    (strategized_player : strategized_player) =
  if strategized_player.player.id != id then strategized_player
  else new_strategized_player strategized_player

let update_player_table (f : card list -> card list) (sp : strategized_player) =
  { sp with player = { sp.player with table = f sp.player.table } }

let update_player_hand (f : card list -> card list) (sp : strategized_player) =
  { sp with hand = f sp.hand }

let update_player_dessert (d : dessert) (sp : strategized_player) =
  { sp with player = { sp.player with desserts = d :: sp.player.desserts } }

let update_player_score (p : Points.points) (sp : strategized_player) =
  { sp with player = { sp.player with score = sp.player.score + p } }

(** Remove a card from a player's hand *)
let remove_card_from_player_hand sps id card =
  update_player_hand (remove_card_from_cards_list card)
  |> update_strategized_player_with_id id
  |> map_strategized_players sps

(** Updates a player's table adding the card they asked to play *)
let put_card_on_player_table (sps : strategized_player list) (id : int)
    (c : card) =
  let sps_after_putting_card =
    update_player_table (put_card_on_table c)
    |> update_strategized_player_with_id id
    |> map_strategized_players sps
  in
  remove_card_from_player_hand sps_after_putting_card id c

(** Updates a player's desserts adding the dessert they asked to play *)
let put_card_in_player_desserts (sps : strategized_player list) (id : int)
    (d : dessert) =
  let sps_after_putting_dessert =
    update_player_dessert d
    |> update_strategized_player_with_id id
    |> map_strategized_players sps
  in
  remove_card_from_player_hand sps_after_putting_dessert id (Dessert d)

(** Removes played [MisoSoup] when more than one player played one *)
let remove_excess_miso_soup (sps, ts) =
  if ts.played_miso_soup = 0 then (sps, ts)
  else
    let players_with_miso =
      List.filter
        (fun (_, c) -> CardType.card_type_of_card c = CardType.MisoSoup)
        ts.miso_and_ura_to_place
    in
    ( (if ts.played_miso_soup == 1 then
         let sp, c = List.hd players_with_miso in
         put_card_on_player_table sps sp.player.id c
       else
         List.fold_left
           (fun sps (sp, c) -> remove_card_from_player_hand sps sp.player.id c)
           sps players_with_miso),
      {
        ts with
        miso_and_ura_to_place =
          ts.miso_and_ura_to_place
          |> List.filter (fun (_, card) -> card != Appetizer MisoSoup);
      } )

(** Returns the card list where the first card matching the given card has been flipped. *)
let flip_first (card_to_flip : card) (cards : card list) =
  let rec ff table = function
    | [] -> table
    | card :: cards when card = card_to_flip -> (
        match card_to_flip with
        | FaceDown _ -> table
        | card -> (FaceDown card :: table) @ cards)
    | card :: cards -> ff (card :: table) cards
  in
  ff [] cards

(** Given a [card list] A and a [card list] B. For all cards in B, place face down a equal card in A. *)
let flip_over_cards cards_to_flip cards =
  List.fold_left
    (fun cards to_flip -> flip_first to_flip cards)
    cards cards_to_flip

(** Given a player id and a [card list], flip all cards on the player's table also present in the given card list. *)
let flip_player_cards (sps : strategized_player list) (id : int)
    (cards_to_flip : card list) =
  update_player_table (flip_over_cards cards_to_flip)
  |> update_strategized_player_with_id id
  |> map_strategized_players sps

(** In a given [card list], replace the first occurence of ~this card ~by that other card. *)
let replace ~this:(card_to_replace : card) ~by:(replacing_card : card) cards =
  Utils.find_index (( = ) card_to_replace) cards |> function
  | None -> cards
  | Some index_to_replace ->
      List.mapi
        (fun i c -> if i <> index_to_replace then c else replacing_card)
        cards

(** Given a [strategized_player list], two indexes `~id_player_hand` and `~id_player_table` and two cards 
    `~card_in_hand` and  `~card_on_table`, swap the card in `player i`'s hand and `player j`'s table. *)
let swap (sps : strategized_player list) ~id_player_hand:(i : int)
    ~id_player_table:(j : int) ~card_in_hand:(hand_card : card)
    ~card_on_table:(table_card : card) =
  (fun sp ->
    match sp.player.id with
    | id when id = i && id = j ->
        let sp =
          update_player_table (replace ~this:table_card ~by:hand_card) sp
        in
        { sp with hand = replace ~this:hand_card ~by:table_card sp.hand }
    | id when id = i ->
        { sp with hand = replace ~this:hand_card ~by:table_card sp.hand }
    | id when id = j ->
        update_player_table (replace ~this:table_card ~by:hand_card) sp
    | _ -> sp)
  |> map_strategized_players sps

(** Go through a list of card that can be placed immediately without side effects on 
    a player's table *)
let place_classic_cards ((sps, ts) : strategized_player list * turn_status) =
  let sps, ts =
    List.fold_left
      (fun (sps, ts) (sp, c) ->
        match c with
        | Dessert d -> (put_card_in_player_desserts sps sp.player.id d, ts)
        | Special (Chopsticks _) ->
            ( put_card_on_player_table sps sp.player.id c,
              {
                ts with
                playing_chopsticks = (sp.player.id, c) :: ts.playing_chopsticks;
              } )
        | Special (Spoon _) ->
            ( put_card_on_player_table sps sp.player.id c,
              { ts with playing_spoon = (sp.player.id, c) :: ts.playing_spoon }
            )
        | _ -> (put_card_on_player_table sps sp.player.id c, ts))
      (sps, ts) ts.classic_cards_to_place
  in
  (sps, { ts with classic_cards_to_place = [] })

(** One try of asking a player what they would like to copy using the Special Order *)
let try_to_choose_what_to_copy gs sp previous_choice _ =
  match previous_choice with
  | None ->
      let choice = sp.strategy.choose_card_to_copy gs sp.player in
      if assert_player_choices_are_in_bounds sp.hand [ choice ] then Some choice
      else None
  | c -> c

(** Asks the player what they would like to copy using a Special Order, but letting them
 have multiple tries if they do not answer correctly. *)
let ask_player_what_they_want_to_copy (gs : game_status) sp =
  List.init gs.number_of_tries (fun _ -> ())
  |> List.fold_left (try_to_choose_what_to_copy gs sp) None
  |> function
  | None -> sp.hand |> List.hd
  | Some card -> card

(** Plays all specials order. If a special order copy a dessert then increment the counter in [turn_status] *)
let special_order_turn (gs : game_status)
    ((sps, ts) : strategized_player list * turn_status) (sp, _) =
  if Utils.is_empty sp.player.table then
    (put_card_on_player_table sps sp.player.id (Special SpecialOrder), ts)
  else
    ask_player_what_they_want_to_copy gs sp |> function
    | Dessert d ->
        ( put_card_in_player_desserts sps sp.player.id d,
          increment_spec_ord_copying_desserts ts )
    | Appetizer MisoSoup ->
        ( sps,
          add_miso_ura_cards_to_queue
            (ts |> increment_miso_soup_counter)
            sp (Appetizer MisoSoup) )
    | SushiRoll (Uramaki n) ->
        (sps, add_miso_ura_cards_to_queue ts sp (SushiRoll (Uramaki n)))
    | copied_card -> (put_card_on_player_table sps sp.player.id copied_card, ts)

(** Copy a card already placed on a player's table *)
let play_special_order (gs : game_status)
    ((sps, ts) : strategized_player list * turn_status) =
  List.fold_left (special_order_turn gs) (sps, ts) ts.special_order_to_use
  |> fun (sps, ts) -> (sps, { ts with special_order_to_use = [] })

(** One try of asking a player what they would like to pick in their hand. *)
let try_to_choose_what_to_pick gs sp ps previous_choice _ =
  match previous_choice with
  | None -> (
      sp.strategy.play_chopsticks gs ps |> function
      | None -> Some None
      | Some card ->
          Some
            (if assert_player_choices_are_in_bounds sp.hand [ card ] then
               Some card
             else None))
  | c -> c

(** Asks the player what they would like to pick in their hand using Chopsticks, but letting them
  have multiples tries if they do not answer correctly. *)
let ask_player_what_they_want_to_pick_in_hand (gs : game_status)
    (sp : strategized_player) (ps : player_status) =
  List.init gs.number_of_tries (fun _ -> ())
  |> List.fold_left (try_to_choose_what_to_pick gs sp ps) None
  |> function
  | None -> None
  | Some card -> card

(** PLay chopsticks, or not, accordingly with what the player choosed. *)
let chopsticks_turn (gs : game_status) (ts : turn_status) sps sp chopsticks =
  if Utils.is_empty sp.hand then (sps, ts)
  else
    let player_status = build_player_status sp in
    ask_player_what_they_want_to_pick_in_hand gs sp player_status |> function
    | None -> (sps, ts)
    | Some card ->
        ( swap sps ~id_player_hand:sp.player.id ~id_player_table:sp.player.id
            ~card_in_hand:card ~card_on_table:chopsticks,
          ts )

(** For a [strategized_player list] and a given [strategized_player], create a [strategized_player list] in a clockwise direction. 
    Assuming that for an index `i`, the player at index `i+1` is to the left of the list. *)
let create_clockwise_order (players : strategized_player list)
    (player : strategized_player) : strategized_player list =
  let players_right, players_left =
    players
    |> List.filter (fun p -> p.player.id <> player.player.id)
    |> List.partition (fun p -> p.player.id > player.player.id)
  in
  players_left @ players_right

(** Given a [spoon_choice] and [strategized_player], return the index of the first player
    whose hand contains cards matching the spoon choice. *)
let index_first_player_owning_requested_card (spoon_choice : spoon_choice)
    (order : strategized_player list) : int option =
  Utils.find_index
    (fun p ->
      List.exists
        (fun card ->
          match spoon_choice with
          | Specific c -> card = c
          | Generic g -> CardType.card_type_of_card card = g)
        p.hand)
    order

(** Given a [strategized_player] and a [spoon_choice], returns all the cards matching the spoon choice. *)
let get_choices_for_spoon (owner : strategized_player)
    (spoon_choice : spoon_choice) =
  List.filter
    (fun card ->
      match spoon_choice with
      | Specific c -> card = c
      | Generic g -> CardType.card_type_of_card card = g)
    owner.hand

(** Given a [strategized_player] and a [card list] containing at least one element, return one of the elements of the list according to the player's choice if it is given to them. 
    @raises Invalid_argument if the list is empty *)
let let_player_choose_card (game_status : game_status)
    (owner : strategized_player) ~options =
  match options with
  | [] ->
      raise (Invalid_argument "Options should contains at least one card")
      (* Unreachable *)
  | [ only_choice ] -> only_choice
  | options ->
      owner.strategy.choose_card_to_give game_status owner.player ~options

(** Remove duplicates elements in a list *)
let remove_duplicates l =
  let rec rd acc = function
    | [] -> acc
    | h :: t -> rd (if List.mem h acc then acc else h :: acc) t
  in
  rd [] l

(** Exchange the spoon card and the card choosed by the player whose being stolen. *)
let proceed_to_exchange (game_status : game_status) (turn_status : turn_status)
    (players : strategized_player list) (player : strategized_player)
    (player_index_owning_request : int) (order : strategized_player list)
    (spoon_choice : spoon_choice) played_spoon =
  (* Compute the player owning the card, the cards that can be traded and the card they want to trade *)
  let owner = List.nth order player_index_owning_request in
  let can_be_choosed =
    get_choices_for_spoon owner spoon_choice |> remove_duplicates
  in
  let card_choosed =
    let_player_choose_card game_status owner ~options:can_be_choosed
  in
  if not (assert_player_choices_are_in_bounds owner.hand [ card_choosed ]) then
    raise (Failure "Player choice is not in their hand.")
  else
    ( swap players ~id_player_hand:owner.player.id
        ~id_player_table:player.player.id ~card_in_hand:card_choosed
        ~card_on_table:played_spoon,
      turn_status )

(** Given a [strategized_player list], a player id and a [Spoon] card, remove the first same spoon in the table of the player. *)
let remove_spoon_from_player (players : strategized_player list) (id : int)
    played_spoon =
  update_player_table (fun t ->
      remove_n_cards_of_type ~strict:true t ~total_to_remove:1 played_spoon)
  |> update_strategized_player_with_id id
  |> map_strategized_players players

(** When a player want to use the spoon, given the [spoon_choice] search among the other players in a particular order if one of them owns a card matching the spoon choice *)
let player_want_to_use_spoon (game_status : game_status)
    (turn_status : turn_status) (players : strategized_player list)
    (player : strategized_player) played_spoon (spoon_choice : spoon_choice) =
  let clockwise_order = create_clockwise_order players player in
  let index_first_player_owning_requested_card =
    index_first_player_owning_requested_card spoon_choice clockwise_order
  in
  match index_first_player_owning_requested_card with
  | None ->
      ( remove_spoon_from_player players player.player.id played_spoon,
        turn_status )
  | Some player_index_owning_request ->
      proceed_to_exchange game_status turn_status players player
        player_index_owning_request clockwise_order spoon_choice played_spoon

(** Given a player who owns a [spoon] card, asks them if they want to play it and act accordingly. *)
let spoon_turn (gs : game_status) (ts : turn_status)
    (sps : strategized_player list) (sp : strategized_player) played_spoon =
  (* No need to sanitize the input, as the player has free choice,if they are
     wrong, they will simply get nothing :) *)
  build_player_status sp |> sp.strategy.play_spoon gs |> function
  | None -> (sps, ts)
  | Some spoon_choice ->
      player_want_to_use_spoon gs ts sps sp played_spoon spoon_choice

let ask_player_what_they_want_to_pick_in_deck (gs : game_status)
    (sp : strategized_player) options previous_choice _ =
  match previous_choice with
  | None ->
      let c = sp.strategy.choose_card_from_deck gs sp.player ~options in
      if assert_player_choices_are_in_bounds options [ c ] then Some c else None
  | c -> c

(** PLay Menu, accordingly with what the player choosed to pick in the deck. *)
let menu_turn (gs : game_status) (ts : turn_status) sps sp original_menu_card =
  let options, _ = get_n_cards_from_deck ts.deck 4 in
  if Utils.is_empty options then (sps, ts)
  else
    let card =
      List.init gs.number_of_tries (fun _ -> ())
      |> List.fold_left
           (ask_player_what_they_want_to_pick_in_deck gs sp options)
           None
      |> function
      | None -> List.hd options
      | Some c -> c
    in
    let ts =
      match card with
      | Special (Menu _) | Special (TakeOutBox _) ->
          add_action_card_to_queue action_sort_function ts sp card
      | Special SpecialOrder -> add_special_order_to_queue ts sp card
      | _ -> ts
    in
    ( remove_card_from_player_hand sps sp.player.id original_menu_card,
      {
        ts with
        deck = remove_n_cards_of_type_from_deck ts.deck ~total_to_remove:1 card;
      } )

(** One try of asking the player what they would like to flip on their table using TakeOutBox. *)
let ask_player_what_they_want_to_flip gs sp previous_choice _ =
  match previous_choice with
  | None ->
      let c = sp.strategy.choose_cards_to_flip gs sp.player in
      if assert_player_choices_are_in_bounds sp.player.table c then Some c
      else None
  | c -> c

(** Given a player who choosed to play [TakeOutBox], ask them which cards they want to flip among the cards on their table. *)
let takeout_turn game_status turn_status players player card =
  let cards_to_flip =
    List.init game_status.number_of_tries (fun _ -> ())
    |> List.fold_left
         (ask_player_what_they_want_to_flip game_status player)
         None
    |> function
    | None -> []
    | Some cards -> cards
  in
  let players = flip_player_cards players player.player.id cards_to_flip in
  (remove_card_from_player_hand players player.player.id card, turn_status)

let use_correctly_action_cards gs (sps, ts) (sp, c) =
  match c with
  | Special (Chopsticks _) -> chopsticks_turn gs ts sps sp c
  | Special (Spoon _) -> spoon_turn gs ts sps sp c
  | Special (Menu _) -> menu_turn gs ts sps sp c
  | Special (TakeOutBox _) -> takeout_turn gs ts sps sp c
  | _ -> raise (Failure "Unreachable")

let remove_head = function [] -> [] | _ :: t -> t

let compare_actions a1 a2 =
  let p1 = get_priority_from_card (snd a1) in
  let p2 = get_priority_from_card (snd a2) in
  compare p1 p2

let sort_actions ts =
  {
    ts with
    action_cards_to_use = List.sort compare_actions ts.action_cards_to_use;
  }

(** For all actions cards placed in the queue, proceed to their associated action. *)
let use_action_cards (gs : game_status) (sps, ts) =
  let rec loop_over_actions (sps, ts) =
    (* This handles priority order *)
    let ts = sort_actions ts in
    if Utils.is_empty ts.action_cards_to_use then (sps, ts)
    else
      loop_over_actions
        (use_correctly_action_cards gs
           ( sps,
             {
               ts with
               action_cards_to_use = remove_head ts.action_cards_to_use;
             } )
           (List.hd ts.action_cards_to_use))
  in
  loop_over_actions (sps, ts)

let increment_points (id : int) (points : Points.points)
    (players : strategized_player list) =
  update_player_score points
  |> update_strategized_player_with_id id
  |> map_strategized_players players

(** Remove all occurences of Uramakis cards on a player's table. *)
let remove_uramaki_cards (id : int) (players : strategized_player list) =
  update_player_table (fun t ->
      remove_n_cards_of_type t ~total_to_remove:max_int (SushiRoll (Uramaki 0)))
  |> update_strategized_player_with_id id
  |> map_strategized_players players

(** Sum all Uramakis icons of player's Uramakis. *)
let count_uramakis_icons player =
  List.fold_left
    (fun count -> function SushiRoll (Uramaki x) -> count + x | _ -> count)
    0 player.player.table

let check_uramakis (internal_game_status : internal_game_status)
    (players, turn_status) : strategized_player list * turn_status =
  let players_with_more_than_10_uramakis =
    turn_status.miso_and_ura_to_place
    |> List.filter_map (fun (p, c) ->
           if CardType.card_type_of_card c = CardType.Uramaki then Some p
           else None)
    |> List.filter (fun player -> count_uramakis_icons player >= 10)
  in
  let players_and_positions =
    players_with_more_than_10_uramakis
    |> List.map (fun player -> count_uramakis_icons player)
    |> Points.get_positions
  in
  Points.uramaki_points players_and_positions
    internal_game_status.played_uramakis
  |> List.map2
       (fun player (_, points) -> (player, points))
       players_with_more_than_10_uramakis
  |> List.fold_left
       (fun (players, ts) (player, points) ->
         ( players
           |> increment_points player.player.id points
           |> remove_uramaki_cards player.player.id,
           increment_played_uramakis_counter ts ))
       (players, turn_status)

(** One try asking a player what they would like to choose in a turn. *)
let try_to_choose_a_first_card (gs : game_status) (sp : strategized_player)
    player_status previous_choice _ =
  match previous_choice with
  | None ->
      let choice = sp.strategy.choose_card gs player_status in
      if assert_player_choices_are_in_bounds sp.hand [ choice ] then Some choice
      else None
  | c -> c

(** Asks a player what they would like to play in a turn, giving them multiples tries. *)
let ask_player_what_they_want_to_choose_first (gs : game_status)
    (sp : strategized_player) player_status =
  List.init gs.number_of_tries (fun _ -> ())
  |> List.fold_left (try_to_choose_a_first_card gs sp player_status) None
  |> function
  | None -> sp.hand |> List.hd
  | Some card -> card

(** Given a [game_status], a [turn_status] and a [strategized_player], ask the player which card they want to play. *)
let player_turn (gs : game_status) (ts : turn_status) (sp : strategized_player)
    =
  let player_status = build_player_status sp in
  let card = ask_player_what_they_want_to_choose_first gs sp player_status in
  match card with
  | SushiRoll (Uramaki _) -> add_miso_ura_cards_to_queue ts sp card
  | Appetizer MisoSoup ->
      add_miso_ura_cards_to_queue ts sp card |> increment_miso_soup_counter
  | Special SpecialOrder -> add_special_order_to_queue ts sp card
  | Special (Menu _) | Special (TakeOutBox _) ->
      add_action_card_to_queue action_sort_function ts sp card
  | _ -> add_classic_card_to_queue ts sp card

(** Given an [internal_game_status] and a function asking a player what they want to play, 
    make all player go through the function then remove cards that cannot be played and add cards that can be played in bonus. *)
let turn_status_based_on_what_players_want_to_play (igs : internal_game_status)
    (make_a_player_choose_a_card :
      turn_status -> strategized_player -> turn_status) =
  igs.players
  |> List.fold_left make_a_player_choose_a_card (initial_turn_status igs)

(** Place all classic cards, then the Special Orders and finally all the actions. *)
let compute_played_cards gs ts sps =
  let sps, ts = place_classic_cards (sps, ts) |> play_special_order gs in
  let ts = add_spoon_chopsticks_to_queue sps ts in
  use_action_cards gs (sps, ts)

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

(** Play a turn of Sushi Go Party. *)
let play_turn ~number_of_tries (internal_game_status : internal_game_status) :
    internal_game_status =
  let game_status =
    game_status_of_internal_game_status internal_game_status ~number_of_tries
  in
  let turn_status =
    turn_status_based_on_what_players_want_to_play internal_game_status
      (player_turn game_status)
  in
  let players, turn_status =
    compute_played_cards game_status turn_status internal_game_status.players
    |> check_uramakis internal_game_status
    |> remove_excess_miso_soup
  in
  {
    internal_game_status with
    players = pass_hands players;
    played_uramakis =
      internal_game_status.played_uramakis + turn_status.played_uramakis;
    total_special_order_copying_desserts =
      internal_game_status.total_special_order_copying_desserts
      + turn_status.special_order_copying_desserts;
    deck = turn_status.deck;
  }
  |> advance_one_turn

(** Advance one round. They current turn is reset to 1. *)
let advance_one_round (internal_game_status : internal_game_status) :
    internal_game_status =
  {
    internal_game_status with
    players =
      List.map
        (fun p -> { p with hand = []; player = { p.player with table = [] } })
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
  List.filter (fun player -> player.id != id) players

(** Find a player with a certain id in a list of players 
   @raise Not_found if no players have that id *)
let find_player_with_id players id =
  List.find (fun player -> player.id = id) players

(** Reforms [strategized_player list] from [player list] and [strategized_player list] 
    @raise Invalid_argument when list have not the same length.
    @raise Not_found when there is [player] who is not is the [strategized_player list]
    *)
let zip_to_strategized_players (players : player list)
    (strategized_players : strategized_player list) : strategized_player list =
  if List.length players != List.length strategized_players then
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
let play_round (internal_game_status : internal_game_status) ~number_of_tries :
    internal_game_status =
  let number_of_turns =
    number_of_cards_to_deal
      ~nb_players:(List.length internal_game_status.players)
  in
  let rec round_loop (internal_game_status : internal_game_status) =
    if internal_game_status.current_turn > number_of_turns then
      internal_game_status
    else play_turn ~number_of_tries internal_game_status |> round_loop
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
let arena ?(number_of_tries = 3) (game_settings : game_settings) =
  let internal_game_status = initial_game_status game_settings in
  let rec loop (internal_game_status : internal_game_status) =
    if internal_game_status.current_round > 3 then internal_game_status
    else play_round internal_game_status ~number_of_tries |> loop
  in
  let post_rounds_internal_game_status = loop internal_game_status in
  post_rounds_internal_game_status.players
  |> List.map (fun { player; _ } -> player)
  |> Points.count_dessert_points |> game_ending_of_player_list
