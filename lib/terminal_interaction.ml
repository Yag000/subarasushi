open Strategies
open Arena
open Cards
open Display
open Player

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

let request_yes_or_no question =
  let message = Format.sprintf "%s (0: No, 1: Yes)" question in
  request_number_in_range message 0 1

let pick_cards_to_flip (card_list : card list) =
  let rec aux acc =
    if List.length acc = List.length card_list then acc
    else
      let msg =
        Format.sprintf
          "@.Pick the cards to flip FcDown by entering a number between %d and \
           %d (inclusive).@.    Enter `0` when you have finished."
          0 (List.length card_list)
      in
      match request_number_in_range msg 0 (List.length card_list) with
      | 0 -> acc
      | i ->
          if not (List.mem (i - 1) acc) then aux ((i - 1) :: acc)
          else (
            Format.printf "This card has already been chosen to be flipped.";
            aux acc)
  in
  let idx_picks = aux [] |> List.fast_sort ( - ) in
  List.filteri (fun idx _ -> List.mem idx idx_picks) card_list

let pick_card_in_card_list (card_list : card list) =
  let length = List.length card_list in
  let message =
    Format.sprintf
      "Please pick a card by choosing a number between %d and %d (inclusive)." 1
      length
  in
  let number = request_number_in_range message 1 length in
  List.nth card_list (number - 1)

let show_game_status (game_status : game_status) =
  Format.printf
    "================================================================================@.";
  pp_game_status Format.std_formatter game_status;
  Format.print_newline ()

let show_info_before_picking (game_status : game_status) (player : player)
    (msg : string) (card_list : card list) =
  Format.printf
    "================================================================================@.";
  pp_game_status Format.std_formatter game_status;
  Format.print_newline ();
  Format.printf "~~~[NOW PLAYING]~~~@.";
  pp_player Format.std_formatter player;
  Format.print_newline ();
  pp_titled_card_list msg Format.std_formatter card_list

let handle_spoon_nigiri () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Nigiri@.    2) Specific Nigiri"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Nigiri
  else
    let msg2 =
      Format.sprintf
        "Which one do you want ? :@.    1) Nigiri Egg@.    2) Nigiri \
         Salmon@.    3) Nigiri Squid"
    in
    match request_number_in_range msg2 1 3 with
    | 1 -> Specific (Nigiri Egg)
    | 2 -> Specific (Nigiri Salmon)
    | 3 -> Specific (Nigiri Squid)
    | _ -> failwith "Impossible"

let handle_spoon_maki () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Maki@.    2) Specific Maki"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Maki
  else
    let msg2 =
      Format.sprintf
        "Which Maki do you want ? :@.    1) Maki 1@.    2) Maki 2@.    3) Maki \
         3"
    in
    match request_number_in_range msg2 1 3 with
    | n -> Specific (SushiRoll (Maki n))

let handle_spoon_uramaki () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Uramaki@.    2) Specific \
       Uramaki"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Uramaki
  else
    let msg2 =
      Format.sprintf
        "Which uramaki do you want ? :@.    3) Uramaki 3@.    4) Uramaki \
         4@.    5) Uramaki 5"
    in
    match request_number_in_range msg2 3 5 with
    | n -> Specific (SushiRoll (Uramaki n))

let handle_spoon_sushi_roll = function
  | Maki _ -> handle_spoon_maki ()
  | Uramaki _ -> handle_spoon_uramaki ()
  | _ -> Specific (SushiRoll Temaki)

let handle_spoon_onigiri () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Onigiri@.    2) Specific \
       Onigiri"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Nigiri
  else
    let msg2 =
      Format.sprintf
        "Which shape do you want ? :@.    1) Ongiri Circle@.    2) Ongiri \
         Triangle@.    3) Ongiri Square@.    4) Ongiri Rectangle"
    in
    match request_number_in_range msg2 1 4 with
    | 1 -> Specific (Appetizer (Onigiri Circle))
    | 2 -> Specific (Appetizer (Onigiri Triangle))
    | 3 -> Specific (Appetizer (Onigiri Square))
    | 4 -> Specific (Appetizer (Onigiri Rectangle))
    | _ -> failwith "Impossible"

let handle_spoon_appetizer (appetizer : appetizer) =
  match appetizer with
  | Onigiri _ -> handle_spoon_onigiri ()
  | _ -> Specific (Appetizer appetizer)

let handle_spoon_chopsticks () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Chopsticks@.    2) Specific \
       Chopsticks"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Chopsticks
  else
    let msg2 =
      Format.sprintf
        "Which Chpstk do you want ? :@.    1) Chopsticks 1@.    2) Chopsticks \
         2@.    3) Chopsticks 3"
    in
    match request_number_in_range msg2 1 3 with
    | n -> Specific (Special (Chopsticks n))

let handle_spoon_spoon () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Spoon@.    2) Specific Spoon"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Spoon
  else
    let msg2 =
      Format.sprintf
        "Which Spoon do you want ? :@.    4) Spoon 4@.    5) Spoon 5@.    6) \
         Spoon 6"
    in
    match request_number_in_range msg2 4 6 with
    | n -> Specific (Special (Spoon n))

let handle_spoon_menu () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Menu@.    2) Specific Menu"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Menu
  else
    let msg2 =
      Format.sprintf
        "Which Menu do you want ? :@.    7) Menu 7@.    8) Menu 8@.    9) Menu \
         9"
    in
    match request_number_in_range msg2 7 9 with
    | n -> Specific (Special (Menu n))

let handle_spoon_takeoutbox () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic TakeOutBox@.    2) Specific \
       TakeOutBox"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.TakeOutBox
  else
    let msg2 =
      Format.sprintf
        "Which TakeOutBox do you want ? :@.    10) TakeOutBox 10@.    11) \
         TakeOutBox 11@.    12) TakeOutBox 12"
    in
    match request_number_in_range msg2 10 12 with
    | n -> Specific (Special (TakeOutBox n))

let handle_spoon_special (special : special) =
  match special with
  | SoySauce -> Specific (Special SoySauce)
  | SpecialOrder -> Specific (Special SpecialOrder)
  | Tea -> Specific (Special Tea)
  | Wasabi _ -> Specific (Special (Wasabi None))
  | Chopsticks _ -> handle_spoon_chopsticks ()
  | Spoon _ -> handle_spoon_spoon ()
  | Menu _ -> handle_spoon_menu ()
  | TakeOutBox _ -> handle_spoon_takeoutbox ()

let handle_spoon_fruit () =
  let msg1 =
    Format.sprintf
      "Which one do you want ? :@.    1) Generic Fruit@.    2) Specific Fruit"
  in
  let nb = request_number_in_range msg1 1 2 in
  if nb = 1 then Generic CardType.Fruit
  else
    let msg2 =
      Format.sprintf
        "Which one do you want ? :@.    1) Fruit [ Orange; Orange ]@.    2) \
         Fruit [ Orange; Watermelon ]@.    3) Fruit [ Orange; Pineapple ]@.    \
         4) Fruit [ Watermelon; Watermelon ]@.    5) Fruit [ Watermelon; \
         Pineapple ]@.    6) Fruit [ Pineapple; Pineapple ]"
    in
    match request_number_in_range msg2 1 6 with
    | 1 -> Specific (Dessert (Fruit [ Orange; Orange ]))
    | 2 -> Specific (Dessert (Fruit [ Orange; Watermelon ]))
    | 3 -> Specific (Dessert (Fruit [ Orange; Pineapple ]))
    | 4 -> Specific (Dessert (Fruit [ Watermelon; Watermelon ]))
    | 5 -> Specific (Dessert (Fruit [ Watermelon; Pineapple ]))
    | 6 -> Specific (Dessert (Fruit [ Pineapple; Pineapple ]))
    | _ -> failwith "Impossible"

let handle_spoon_dessert (dessert : dessert) =
  match dessert with
  | Fruit _ -> handle_spoon_fruit ()
  | d -> Specific (Dessert d)

let do_spoon_choice (r, app1, app2, app3, sp1, sp2, d) =
  let msg =
    Format.sprintf
      "From the menu, pick a card by choosing a number between %d and %d \
       (inclusive)."
      1 8
  in
  let nb = request_number_in_range msg 1 8 in
  match nb with
  | 1 -> handle_spoon_nigiri ()
  | 2 -> r |> handle_spoon_sushi_roll
  | 3 -> app1 |> handle_spoon_appetizer
  | 4 -> app2 |> handle_spoon_appetizer
  | 5 -> app3 |> handle_spoon_appetizer
  | 6 -> sp1 |> handle_spoon_special
  | 7 -> sp2 |> handle_spoon_special
  | 8 -> d |> handle_spoon_dessert
  | _ -> failwith "Impossible"

(** A player that gets its input from the terminal. *)
let teletype_player : player_strategy =
  {
    choose_card =
      (fun game_status player_status ->
        show_game_status game_status;
        Format.printf "~~~[NOW PLAYING]~~~@.";
        pp_player_status Format.std_formatter player_status;
        Format.print_newline ();
        pick_card_in_card_list player_status.hand);
    play_chopsticks =
      (fun game_status player_status ->
        show_game_status game_status;
        Format.printf "~~~[NOW PLAYING]~~~@.";
        pp_player_status Format.std_formatter player_status;
        Format.print_newline ();
        let question = "Do you want to play Chpstk ?" in
        let choice = request_yes_or_no question in
        match choice with
        | 0 -> None
        | 1 -> Some (pick_card_in_card_list player_status.hand)
        | _ -> failwith "Impossible");
    play_spoon =
      (fun game_status player_status ->
        show_game_status game_status;
        Format.printf "~~~[NOW PLAYING]~~~@.";
        pp_player_status Format.std_formatter player_status;
        Format.print_newline ();
        let question = "Do you want to play Spoon ?" in
        let choice = request_yes_or_no question in
        match choice with
        | 0 -> None
        | 1 -> Some (do_spoon_choice game_status.menu)
        | _ -> failwith "Impossible");
    choose_card_from_deck =
      (fun game_status player ~options ->
        show_info_before_picking game_status player "Pick one option" options;
        pick_card_in_card_list options);
    choose_card_to_copy =
      (fun game_status player ->
        show_info_before_picking game_status player "Copy one option"
          player.table;
        pick_card_in_card_list player.table);
    choose_cards_to_flip =
      (fun game_status player ->
        show_info_before_picking game_status player "Here are your options"
          player.table;
        pick_cards_to_flip player.table);
    choose_card_to_give =
      (fun game_status player ~options ->
        show_info_before_picking game_status player "Choose a card to give"
          options;
        pick_card_in_card_list options);
  }

(** Ask a user for a string. *)
let request_string message =
  Format.printf "%s@." message;
  Format.print_flush ();
  read_line ()

(** Greets the user. *)
let greet_user () = Format.printf "Welcome to Sushi Go Party!@."

type player_number_info = { num_players : int; num_bots : int }
(** Number of player and bots. *)

(** Ask a user for the number of players and bots. *)
let request_number_of_players () =
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
let get_players () =
  let player_number_info = request_number_of_players () in
  let names =
    List.init player_number_info.num_players (fun i ->
        (teletype_player, get_player_name (i + 1)))
  in
  let bots =
    List.init player_number_info.num_bots (fun i ->
        (random_player, "Bot " ^ string_of_int (i + 1)))
  in
  names @ bots

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
    "Which menu would you like to play with?@.1) MyFirstMeal@.2) SushiGo @.3) \
     PartySampler (max 6 players)@.4) MasterMenu@.5) PointsPlatter@.6) \
     CutthroatCombo@.7) BigBanquet@.8) DinnerForTwo (max 6 plyers) @.9) Custom"

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
  greet_user ();
  let players = get_players () in
  let total_player_number = List.length players in
  let menu = choose_menu total_player_number in
  { players; menu }
