open Strategies
open Arena
open Cards
open Display

let unimplemented2 _ _ = failwith "unimplemented"
let unimplemented_options _ _ ~options:_ = failwith "unimplemented"

(** A player that gets its input from the terminal. *)
let teletype_player : player_strategy =
  {
    choose_card = unimplemented2;
    play_chopsticks = unimplemented2;
    play_spoon = unimplemented2;
    choose_card_from_deck = unimplemented_options;
    choose_card_to_copy = unimplemented2;
    choose_cards_to_flip = unimplemented2;
    choose_card_to_give = unimplemented_options;
  }

(** Ask a user for a number between [a] and [b] (inclusive). *)
let rec request_number_in_range message a b =
  Format.printf "%s@." message;
  Format.print_flush ();
  try
    let n = read_int () in
    if n < a || n > b then (
      Format.printf
        "Invalid input, please enter a number between %d and %d (inclusive).@."
        a b;
      request_number_in_range message a b)
    else n
  with Failure _ ->
    Format.printf "Invalid input.@.";
    request_number_in_range message a b

(** Ask a user for a string. *)
let request_string message =
  Format.printf "%s@." message;
  Format.print_flush ();
  read_line ()

(** Greets the user. *)
let greet_user = Format.printf "Welcome to Sushi Go Party!@."

type player_number_info = { num_players : int; num_bots : int }
(** Number of player and bots. *)

(** Ask a user for the number of players and bots. *)
let request_number_of_players =
  let message = "How many players are there? (2-8)" in
  let num_players = request_number_in_range message 2 8 in
  let message =
    Format.sprintf
      "Of those %d players, how many of them are random bots? (0-%d)"
      num_players num_players
  in
  let num_bots = request_number_in_range message 0 num_players in
  { num_players = num_players - num_bots; num_bots }

(** Ask a user for the name of a player. *)
let get_player_name i =
  let message = Format.sprintf "What is the name of player %d?" i in
  request_string message

(** Returns the list of players, with the firsts being human (if 
    applicable), the rest being bots. *)
let get_players =
  let player_number_info = request_number_of_players in
  let names =
    List.init player_number_info.num_players (fun i ->
        (teletype_player, get_player_name (i + 1)))
  in
  let bots =
    List.init player_number_info.num_bots (fun i ->
        (random_player, "Bot " ^ string_of_int (i + 1)))
  in
  names @ bots

(** Pretty print a card, ignoring its data. *)
let pp_card_ignore_data ff (card : card) =
  match card with
  | Nigiri n -> pp_nigiri ff n
  | SushiRoll sr -> (
      match sr with
      | Maki _ -> Format.fprintf ff "MakiRo"
      | Uramaki _ -> Format.fprintf ff "UrMaki"
      | x -> pp_sushi_roll ff x)
  | Appetizer ap -> (
      match ap with
      | Onigiri _ -> Format.fprintf ff "Oni"
      | x -> pp_appetizer ff x)
  | Special sp -> (
      match sp with
      | Chopsticks _ -> Format.fprintf ff "Chpstk"
      | Spoon _ -> Format.fprintf ff "Spoon"
      | Menu _ -> Format.fprintf ff "Menu"
      | TakeOutBox _ -> Format.fprintf ff "TkoBox"
      | x -> pp_special ff x)
  | Dessert d -> (
      match d with Fruit _ -> Format.fprintf ff "Fruit" | _ -> pp_dessert ff d)
  | FaceDown _ -> Format.fprintf ff "FcDown"

(** Pretty print a list of cards, ignoring their data and adding their position
    in the list. *)
let pp_option_list (l : card list) =
  let rec loop i = function
    | [] -> ""
    | [ h ] -> Format.asprintf "%a (%d)" pp_card_ignore_data h i
    | h :: t ->
        Format.asprintf "%a (%d)@.%s" pp_card_ignore_data h i (loop (i + 1) t)
  in
  loop 1 l

(** Ask a user to choose a card from a list of options. *)
let choose_card_from (options : card list) name =
  let options_size = List.length options in
  let message =
    Format.sprintf "Choose one of the following %s to play with:@.%s" name
      (pp_option_list options)
  in
  request_number_in_range message 1 options_size |> function
  | i when i <= options_size -> List.nth options (i - 1)
  | _ ->
      failwith Printf.(sprintf "Unbound %s number, should be impossible" name)

(** Ask a user to choose [number] cards from a list of options. *)
let choose_cards_from_type number name options =
  List.init number (fun _ -> ())
  |> List.fold_left
       (fun (choosed, remaining) _ ->
         let choosed_ap = choose_card_from remaining name in
         let remaining = List.filter (fun x -> x <> choosed_ap) remaining in
         (choosed_ap :: choosed, remaining))
       ([], options)
  |> fst |> List.rev

(** Ask a user to choose the sushi roll card. *)
let choose_sushi_roll () =
  [ Maki 0; Uramaki 0; Temaki ]
  |> List.map (fun x -> SushiRoll x)
  |> choose_cards_from_type 1 "sushi rolls"
  |> List.map (fun x ->
         match x with SushiRoll y -> y | _ -> failwith "Impossible")
  |> List.hd

(** Ask a user to choose the appetizer cards. *)
let choose_appetizers () =
  [ Dumpling; Edamame; Eel; MisoSoup; Sashimi; Tempura; Tofu; Onigiri Triangle ]
  |> List.map (fun x -> Appetizer x)
  |> choose_cards_from_type 3 "appetizers"
  |> List.map (fun x ->
         match x with Appetizer y -> y | _ -> failwith "Impossible")

(** Ask a user to choose the special cards. *)
let choose_specials player_number =
  [
    Chopsticks 0;
    Menu 0;
    SoySauce;
    Spoon 0;
    SpecialOrder;
    TakeOutBox 0;
    Tea;
    Wasabi None;
  ]
  |> List.filter (fun x ->
         match x with Menu _ -> player_number <= 6 | _ -> true)
  |> List.map (fun x -> Special x)
  |> choose_cards_from_type 2 "specials"
  |> List.map (fun x ->
         match x with Special y -> y | _ -> failwith "Impossible")

(** Ask a user to choose the dessert card. *)
let choose_dessert () =
  [ Pudding; Fruit []; MatchaIceCream ]
  |> List.map (fun x -> Dessert x)
  |> choose_cards_from_type 1 "desserts"
  |> List.hd
  |> function
  | Dessert x -> x
  | _ -> failwith "Impossible"

(** Ask a user to choose a menu custom menu. *)
let choose_custom_menu player_number =
  let sushi_roll = choose_sushi_roll () in
  let appetizers = choose_appetizers () in
  let specials = choose_specials player_number in
  let dessert = choose_dessert () in
  ( sushi_roll,
    List.nth appetizers 0,
    List.nth appetizers 1,
    List.nth appetizers 2,
    List.nth specials 0,
    List.nth specials 1,
    dessert )

(** Menu options *)
let menu_message =
  Format.sprintf
    "Which menu would you like to play with?@.MyFirstMeal (1)@.SushiGo \
     (2)@.PartySampler (max 6 players) (3)@.MasterMenu (4)@.PointsPlatter \
     (5)@.CutthroatCombo (6)@.BigBanquet (7)@.DinnerForTwo (max 6 plyers) \
     (8)@.Custom (9)"

(** Ask a user to choose a menu from a list of options. They can choose 
    a default menu or a custom menu. *)
let rec choose_menu total_player_number =
  let message = menu_message in
  let menu_number = request_number_in_range message 1 9 in
  match menu_number with
  | 1 -> MyFirstMeal |> menu_of_default_menu
  | 2 -> SushiGo |> menu_of_default_menu
  | 3 ->
      if total_player_number > 6 then (
        Format.printf
          "Too many players for PartySampler, pelase choose another menu@.";
        choose_menu total_player_number)
      else PartySampler |> menu_of_default_menu
  | 4 -> MasterMenu |> menu_of_default_menu
  | 5 -> PointsPlatter |> menu_of_default_menu
  | 6 -> CutthroatCombo |> menu_of_default_menu
  | 7 -> BigBanquet |> menu_of_default_menu
  | 8 ->
      if total_player_number > 6 then (
        Format.printf
          "Too many players for DinnerForTwo, pelase choose another menu@.";
        choose_menu total_player_number)
      else DinnerForTwo |> menu_of_default_menu
  | 9 -> choose_custom_menu total_player_number
  | _ -> failwith "Unbound menu number, should be impossible"

(** [get_game_settings ()] prompts the user for the game settings. *)
let get_game_settings () =
  greet_user;
  let players = get_players in
  let total_player_number = List.length players in
  let menu = choose_menu total_player_number in
  { players; menu }
