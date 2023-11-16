open Cards
open Player
open Arena

let pp_nigiri ff (nigiri : nigiri) =
  let variety =
    match nigiri with Egg -> "Egg" | Salmon -> "Slm" | Squid -> "Sqd"
  in
  Format.fprintf ff "Ni%s" variety

let pp_sushi_roll ff (sushi_roll : sushi_roll) =
  match sushi_roll with
  | Temaki -> Format.fprintf ff "Temaki"
  | Maki n -> Format.fprintf ff "MakiRo %d" n
  | Uramaki n -> Format.fprintf ff "UrMaki %d" n

let pp_shape ff (shape : shape) =
  let variety =
    match shape with
    | Circle -> "Cir"
    | Triangle -> "Tri"
    | Square -> "Sqr"
    | Rectangle -> "Rec"
  in
  Format.fprintf ff "%s" variety

let pp_appetizer ff (appetizer : appetizer) =
  match appetizer with
  | Dumpling -> Format.fprintf ff "Dmplng"
  | Edamame -> Format.fprintf ff "Edamam"
  | Eel -> Format.fprintf ff "Eel"
  | MisoSoup -> Format.fprintf ff "MisoSp"
  | Sashimi -> Format.fprintf ff "Sashmi"
  | Tempura -> Format.fprintf ff "Tmpura"
  | Tofu -> Format.fprintf ff "Tofu"
  | Onigiri shape ->
      Format.fprintf ff "Oni";
      pp_shape ff shape

let pp_fruit_kind ff (fruit_kind : fruit_kind) =
  let variety =
    match fruit_kind with Watermelon -> "W" | Orange -> "O" | Pineapple -> "P"
  in
  Format.fprintf ff "%s" variety

let pp_special ff (special : special) =
  match special with
  | Chopsticks n -> Format.fprintf ff "Chpstk %d" n
  | Spoon n -> Format.fprintf ff "Spoon %d" n
  | Menu n -> Format.fprintf ff "Menu %d" n
  | TakeOutBox n -> Format.fprintf ff "TkoBox %d" n
  | SoySauce -> Format.fprintf ff "SoySce"
  | SpecialOrder -> Format.fprintf ff "SpOrdr"
  | Tea -> Format.fprintf ff "Tea"
  | Wasabi opt -> (
      match opt with
      | Some nigiri ->
          Format.fprintf ff "Wsb ";
          pp_nigiri ff nigiri
      | _ -> Format.fprintf ff "Wasabi")

let pp_dessert ff (dessert : dessert) =
  match dessert with
  | MatchaIceCream -> Format.fprintf ff "Matcha"
  | Pudding -> Format.fprintf ff "Puddng"
  | Fruit fruit_kind_list -> (
      match fruit_kind_list with
      | [] -> Format.fprintf ff "Fruit: []"
      | _ ->
          Format.fprintf ff "Fruit: [@[<hov>%a@]]"
            Format.(
              pp_print_list
                ~pp_sep:(fun out () -> fprintf out ";@ ")
                pp_fruit_kind)
            fruit_kind_list)

let pp_facedown ff = Format.fprintf ff "FcDown"

let pp_card ff (card : card) =
  match card with
  | Nigiri nigiri -> pp_nigiri ff nigiri
  | SushiRoll sushi_roll -> pp_sushi_roll ff sushi_roll
  | Appetizer appetizer -> pp_appetizer ff appetizer
  | Special special -> pp_special ff special
  | Dessert dessert -> pp_dessert ff dessert
  | FaceDown _ -> pp_facedown ff

let pp_titled_list (title : string) ff (l : 'a list) pp =
  Format.fprintf ff "%s" title;
  match l with
  | [] -> Format.fprintf ff ": []"
  | _ ->
      Format.fprintf ff ": [@[<hov>%a@]]"
        Format.(pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp)
        l

let pp_titled_card_list (title : string) ff (card_list : card list) =
  pp_titled_list title ff card_list pp_card

let pp_hand = pp_titled_card_list "Hand"

let pp_player ff (player : player) =
  Format.fprintf ff "%s (%d pts)@." player.name player.score;
  Format.fprintf ff "    ";
  pp_titled_card_list "Desserts" ff
    (player.desserts |> List.map (fun d -> Dessert d));
  Format.fprintf ff "@.    ";
  pp_titled_card_list "Table" ff player.table

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

let pp_menu ff (sr, app1, app2, app3, sp1, sp2, dess) =
  Format.fprintf ff "Menu : [Nigiri; ";
  let l =
    [
      SushiRoll sr;
      Appetizer app1;
      Appetizer app2;
      Appetizer app3;
      Special sp1;
      Special sp2;
      Dessert dess;
    ]
  in
  Format.fprintf ff "@[<hov>%a@]]"
    Format.(
      pp_print_list
        ~pp_sep:(fun out () -> fprintf out ";@ ")
        pp_card_ignore_data)
    l

let pp_player_list ff = function
  | [] -> Format.fprintf ff "Players: []"
  | l ->
      Format.fprintf ff "Players: [@.@[<hov>%a@]@.]"
        Format.(
          pp_print_list ~pp_sep:(fun out () -> fprintf out ";@ ") pp_player)
        l

let pp_player_pair ff (name, score) = Format.fprintf ff "%s %d pts" name score

let pp_winners ff = function
  | [] -> Format.fprintf ff "No winner" (* Never happens, Just to be safe *)
  | pair_list ->
      Format.fprintf ff "Winners: [@.@[<hov>%a@]@.]"
        Format.(
          pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_player_pair)
        pair_list

let pp_win ff = function
  | Single pair ->
      Format.fprintf ff "Winner: ";
      pp_player_pair ff pair
  | Draw pair_list -> pp_winners ff pair_list

let pp_leaderboard ff = function
  | [] -> Format.fprintf ff "No player" (* Never happens, Just to be safe *)
  | pair_list ->
      Format.fprintf ff "Leaderboard: [@.@[<hov>%a@]@.]"
        Format.(
          pp_print_list ~pp_sep:(fun out () -> fprintf out "@.") pp_player_pair)
        pair_list

let pp_game_ending ff { winners; players } =
  pp_win ff winners;
  Format.fprintf ff "@.";
  pp_leaderboard ff players

let pp_player_status ff ({ player; hand } : player_status) =
  pp_player ff player;
  Format.fprintf ff "@.    ";
  pp_hand ff hand

let pp_game_status ff (game_status : game_status) =
  Format.fprintf ff "Round: %d@.Turn: %d@." game_status.current_round
    game_status.current_turn;
  pp_menu ff game_status.menu;
  Format.fprintf ff "@.";
  match game_status.players with
  | [] -> Format.fprintf ff "Players: []"
  | l -> pp_player_list ff l
