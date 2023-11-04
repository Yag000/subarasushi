open Cards
open Player

type points = int

(** This type is used to count the points a type of card gives. *)
type point_counter =
  | AmountOfCards of (int -> points)
  (* The first [int] is the amount of cards, the second [int] is the amount of
       opponents that have played the same card. *)
  | AmountOfCardsAndCardsByOpponents of
      (amount_of_cards:int -> cards_played_by_oponents:int -> points)
  (* The first [int] is the maximum amount of cards of the same color a player has
     and the second one is the amount of cards a player has that use this point counter *)
  | CardAmountAndMaxSameColor of
      (amount_of_cards:int -> max_same_color:int -> points)
  | TripleNigiri of (nigiri option -> points)
  | Shapes of (shape list -> points)
  | NoPoints

let point_of_nigiri = function Egg -> 1 | Salmon -> 2 | Squid -> 3

(** Returns the point counter of a card. *)
let point_counter_of_card = function
  | Nigiri card -> AmountOfCards (fun x -> x * point_of_nigiri card)
  | Appetizer Dumpling ->
      AmountOfCards (function 1 -> 1 | 2 -> 3 | 3 -> 6 | 4 -> 10 | _ -> 15)
  | Appetizer Edamame ->
      AmountOfCardsAndCardsByOpponents
        (fun ~amount_of_cards ~cards_played_by_oponents ->
          amount_of_cards * min 4 cards_played_by_oponents)
  | Appetizer Eel -> AmountOfCards (fun x -> if x = 1 then -3 else 7)
  | Appetizer (Onigiri _) ->
      Shapes
        (fun l ->
          let partitioned_list = Utils.partition_list ( = ) l in
          let a, b =
            List.fold_left
              (fun (a, b) x ->
                let l = List.length x in
                ((if l > 0 then a + 1 else a), if l = 2 then b + 1 else b))
              (0, 0) partitioned_list
          in
          (a * a) + (b * b))
  | Appetizer MisoSoup -> AmountOfCards (fun x -> 3 * x)
  | Appetizer Sashimi -> AmountOfCards (fun x -> 10 * (x / 3))
  | Appetizer Tempura -> AmountOfCards (fun x -> 5 * (x / 2))
  | Appetizer Tofu -> AmountOfCards (function 1 -> 2 | 2 -> 6 | _ -> 0)
  | Special Tea ->
      CardAmountAndMaxSameColor
        (fun ~amount_of_cards ~max_same_color ->
          amount_of_cards * max_same_color)
  | Special (Wasabi _) ->
      TripleNigiri
        (function Some card -> 3 * point_of_nigiri card | None -> 0)
  | Special _ -> NoPoints
  | FaceDown _ -> AmountOfCards (fun x -> 2 * x)
  | _ -> NoPoints

(** Checks if two cards are of the same type. 
    This is not the same as structural equality, we do not care about the values of
    the cards, only about their type. For example [Nigiri Egg] and [Nigiri Salmon]
    are not the same, but [Onigiri Triangle] and [Onigiri Circle] are. 
*)
let are_same_type card1 card2 =
  match (card1, card2) with
  | Nigiri n1, Nigiri n2 -> n1 = n2
  | SushiRoll (Maki _), SushiRoll (Maki _) -> true
  | SushiRoll (Uramaki _), SushiRoll (Uramaki _) -> true
  | SushiRoll Temaki, SushiRoll Temaki -> true
  | Appetizer (Onigiri _), Appetizer (Onigiri _) -> true
  | Appetizer a1, Appetizer a2 -> a1 = a2
  | Special (Wasabi _), Special (Wasabi _) -> true
  | Special (Menu _), Special (Menu _) -> true
  | Special (TakeOutBox _), Special (TakeOutBox _) -> true
  | Special (Chopsticks _), Special (Chopsticks _) -> true
  | Special (Spoon _), Special (Spoon _) -> true
  | Special s1, Special s2 -> s1 = s2
  | Dessert (Fruit _), Dessert (Fruit _) -> true
  | Dessert d1, Dessert d2 -> d1 = d2
  | FaceDown _, FaceDown _ -> true
  | _ -> false

(** Checks if two cards have the same background color. *)
let have_same_color card1 card2 =
  are_same_type card1 card2
  ||
  match (card1, card2) with
  | Nigiri _, Nigiri _
  | Special (Wasabi _), Nigiri _
  | Nigiri _, Special (Wasabi _) ->
      true
  | _ -> false

type partitioned_hand = card list list
(** A hand partitioned by card type. *)

(** Partitions a hand by card type. *)
let partitioned_hand_of_hand_list = Utils.partition_list are_same_type

(** Computes the amount of cards of the same type as the given card in the hand. *)
let compute_amount_of_cards card (partitioned_hand : partitioned_hand) =
  List.find_opt
    (fun x -> match x with head :: _ -> are_same_type head card | _ -> false)
    partitioned_hand
  |> function
  | Some x -> List.length x
  | None -> 0

(** Computes the biggest amount of cards that have the same background color in a
    [card list]. *)
let count_max_same_color hand =
  (* Face-down cards do not have any color *)
  List.filter (fun x -> match x with FaceDown _ -> false | _ -> true) hand
  |> Utils.partition_list have_same_color
  |> List.map (fun x ->
         (* Equivalent to [List.length] but counting also the cards that are being held by a [Wasabi] *)
         List.fold_left
           (fun acc card ->
             match card with
             | Special (Wasabi (Some _)) -> acc + 2
             | _ -> acc + 1)
           0 x)
  |> List.fold_left max 0

(** Computes the amount of different background colors in a [card list]. *)
let count_different_colors hand =
  (* Face-down cards do not have any color *)
  List.filter (fun x -> match x with FaceDown _ -> false | _ -> true) hand
  |> Utils.partition_list have_same_color
  |> List.length

type position = First | Second | Third | Other | Last

let shift_position = function
  | First -> Second
  | Second -> Third
  | Third -> Other
  | Other -> Last
  | Last -> First

let rec shift_position_by n position =
  if n = 0 then position
  else shift_position_by (n - 1) (shift_position position)

(** Creates a position list from a [(int * int) list]. The list contains first the
    id of the player and then the value you want to sort on. If two elements are equal,
    they are considered to be in the same position. There is no skipping of positions. *)
let get_positions (players : int list) : (int * position) list =
  let player_number = List.length players in
  let players = List.mapi (fun i x -> (i, x)) players in
  let sorted_players = List.sort (fun (_, x) (_, y) -> compare y x) players in
  let top_points = snd (List.hd sorted_players) in
  let winners =
    List.filter_map
      (fun (x, y) -> if y = top_points then Some x else None)
      sorted_players
  in
  let winner_number = List.length winners in
  let seconds =
    if winner_number >= player_number then []
    else
      List.filter_map
        (fun (x, y) ->
          if y = snd (List.nth sorted_players winner_number) then Some x
          else None)
        sorted_players
  in
  let seconds_number = List.length seconds in
  let thirds =
    (* there are no thirds on a game with less that 6 players *)
    if player_number < 6 || winner_number + seconds_number >= player_number then
      []
    else
      List.filter_map
        (fun (x, y) ->
          if y = snd (List.nth sorted_players (winner_number + seconds_number))
          then Some x
          else None)
        sorted_players
  in
  let last =
    List.filter_map
      (fun (x, y) ->
        if
          y = snd (List.nth sorted_players (player_number - 1))
          && y != top_points
        then Some x
        else None)
      sorted_players
  in
  List.map
    (fun (x, played) ->
      ( x,
        if played = 0 then Last
        else if List.mem x winners then First
        else if List.mem x seconds then Second
        else if List.mem x thirds then Third
        else if List.mem x last then Last
        else Other ))
    players
  |> List.sort (fun (x, _) (y, _) -> compare x y)

(** Finds the first [sushi_roll] of a list of cards. If none is present it defaults to [Uramaki] *)
let find_sushiroll hands =
  hands
  |> List.filter_map (fun cards ->
         List.filter_map (function SushiRoll s -> Some s | _ -> None) cards
         |> function
         | [] -> None
         | s :: _ -> Some s)
  |> function
  (* We use [Uramaki] as they do no add or remove points if nobody
     has played them. In this way if no [sushi_roll] is played, then
        the points are not affected. *)
  | [] -> Uramaki 0
  | s :: _ -> s

(** Computes the amount of sushi roll icons in a cards ([temaki] are worth 1). 
    If the card is not a sushi roll, it returns 0. *)
let count_shushi_roll_icons = function
  | SushiRoll (Maki x) | SushiRoll (Uramaki x) -> x
  | SushiRoll _ -> 1
  | _ -> 0

(** Computes the points won with the [maki] cards. *)
let maki_points l =
  let player_nb = List.length l in
  l
  |> List.map (fun (id, position) ->
         ( id,
           match position with
           | First -> 6
           | Second -> if player_nb >= 6 then 4 else 3
           | Third when player_nb >= 6 -> 2
           | _ -> 0 ))

(** Computes the points won with the [temaki] cards. *)
let temaki_points l =
  let player_nb = List.length l in
  l
  |> List.map (fun (id, position) ->
         ( id,
           match position with
           | First -> 4
           | Last when player_nb > 2 -> -4
           | _ -> 0 ))

(** Computes the points won with the [uramaki] cards. *)
let uramaki_points l played_uramakis =
  let winning_position = shift_position_by played_uramakis First in
  let points_won =
    match winning_position with First -> 8 | Second -> 5 | Third -> 2 | _ -> 0
  in
  l
  |> List.map (fun (id, position) ->
         (id, match position with First -> points_won | _ -> 0))

(** Counts the amount of cards of the same type as the given card in the hand. *)
let count_cards card =
  List.fold_left (fun acc x -> if are_same_type card x then acc + 1 else acc) 0

(** Computes the points won with the [soysauce] cards. *)
let soysauce_points cards =
  let different_colors =
    List.map (fun l -> (l, count_different_colors l)) cards
  in
  let max_colors =
    List.fold_left (fun acc (_, x) -> max acc x) 0 different_colors
  in
  List.map
    (fun (cards, x) ->
      if x = max_colors then 4 * count_cards (Special SoySauce) cards else 0)
    different_colors

(** Computes the points of the shared cards. I refer to the cards that give points
    depending on the position of the player: [Maki], [Uramaki], [Temaki] and [SoySauce]. *)
let compute_shared_points played_uramakis players =
  let indexed_players = List.mapi (fun i x -> (i, x)) players in
  let hands = List.map (fun x -> x.table) players in
  (* We compute the number of sushi roll icons for each player.*)
  let sushi_roll_icons =
    List.map
      (fun l ->
        List.fold_left (fun acc x -> acc + count_shushi_roll_icons x) 0 l)
      hands
  in
  let positions = get_positions sushi_roll_icons in
  let points =
    match find_sushiroll hands with
    | Maki _ -> maki_points positions
    | Uramaki _ -> uramaki_points positions played_uramakis
    | Temaki -> temaki_points positions
  in
  List.combine points (soysauce_points hands)
  |> List.map (fun ((id, p1), p2) -> (List.assoc id indexed_players, p1 + p2))

let count_hand_points hand other_hands =
  let partitioned_hand = partitioned_hand_of_hand_list hand in
  let partitioned_other_hands =
    List.map partitioned_hand_of_hand_list other_hands
  in
  let computes_points_from_card = function
    | [] -> 0
    | h :: t -> (
        point_counter_of_card h |> function
        | AmountOfCards f -> f (List.length t + 1)
        | AmountOfCardsAndCardsByOpponents f ->
            let cards_played_by_oponents =
              List.map
                (fun x -> min 1 (compute_amount_of_cards h x))
                partitioned_other_hands
              |> List.fold_left ( + ) 0
            in
            f ~amount_of_cards:(List.length t + 1) ~cards_played_by_oponents
        | CardAmountAndMaxSameColor f ->
            let max_same_color = count_max_same_color hand in
            f ~amount_of_cards:(List.length t + 1) ~max_same_color
        | TripleNigiri f ->
            (* All the [wasabi] cards should be contained
               in this sub-list (as it was partitioned to do so) *)
            List.fold_left
              (fun acc -> function
                | Special (Wasabi nigiri) -> acc + f nigiri
                | _ -> acc)
              0 (h :: t)
        | Shapes f ->
            List.filter_map
              (function Appetizer (Onigiri x) -> Some x | _ -> None)
              (h :: t)
            |> f
        | NoPoints -> 0)
  in
  List.map computes_points_from_card partitioned_hand |> List.fold_left ( + ) 0

(** Computes the points of a round. It ignores desserts, as they should be counted with the
    function [count_dessert_points]. *)
let count_round_points ~played_uramakis players =
  let hands = List.map (fun x -> x.table) players in
  compute_shared_points played_uramakis players
  |> List.mapi (fun i (player, points) ->
         ( player,
           points
           + count_hand_points (List.nth hands i)
               (Utils.list_remove_index i hands) ))
  |> List.map (fun (player, turn_points) ->
         {
           name = player.name;
           table = player.table;
           desserts = player.desserts;
           score = player.score + turn_points;
         })

(** Computes the points won using the puddings. *)
let compute_pudding_points l =
  let player_nb = List.length l in
  List.map
    (fun (_, position) ->
      match position with First -> 6 | Last when player_nb > 2 -> -6 | _ -> 0)
    l

(** Computes the points won using the matcha ice creams. *)
let compute_matcha_points l =
  List.map
    (fun l ->
      let length = List.length l in
      12 * (length / 4))
    l

(** Computes the points won counting the number of fruits. *)
let points_of_fruit_amount = function
  | 0 -> -2
  | 1 -> 0
  | 2 -> 1
  | 3 -> 3
  | 4 -> 6
  | _ -> 10

let compute_fruit_points (l : fruit_kind list list) =
  let partitioned_hands = List.map (Utils.partition_list ( = )) l in
  List.map
    (fun x ->
      List.fold_left
        (fun acc x -> acc + points_of_fruit_amount (List.length x))
        0 x
      (* This takes care of missing fruits *)
      - ((3 - List.length x) * 2))
    partitioned_hands

(** Finds the first [dessert] of a list of cards. If none is present it defaults to [MatchaIceCream] *)
let find_dessert (desserts : dessert list list) =
  desserts |> List.flatten |> function
  (* We use [MatchaIceCream] as they do not remove points if nobody
     has played them. This way if no [dessert] is played, then
        the points are not affected. *)
  | [] -> MatchaIceCream
  | d :: _ -> d

(** Computes the points won using the desserts. *)
let count_dessert_points players =
  let desserts = List.map (fun x -> x.desserts) players in
  (match find_dessert desserts with
  | Fruit _ ->
      List.map
        (fun fruits ->
          List.filter_map
            (function Fruit kind -> Some kind | _ -> None)
            fruits
          |> List.flatten)
        desserts
      |> compute_fruit_points
  | MatchaIceCream -> compute_matcha_points desserts
  | Pudding ->
      let positions =
        List.map (fun l -> List.length l) desserts |> get_positions
      in
      compute_pudding_points positions)
  |> List.combine players
  |> List.map (fun (player, dessert_points) ->
         {
           name = player.name;
           table = player.table;
           desserts = player.desserts;
           score = player.score + dessert_points;
         })
