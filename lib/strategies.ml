open Arena
open Utils

let random_player =
  {
    choose_card =
      (fun _ player_status ->
        list_random_element_opt player_status.hand |> Option.get);
    play_chopsticks =
      (fun _ player_status ->
        if Random.int 2 = 0 then None
        else list_random_element_opt player_status.hand);
    play_spoon =
      (fun _ player_status ->
        if Random.int 2 = 0 then None
        else
          list_random_element_opt player_status.hand
          |> Option.map (fun x -> Specific x));
    choose_card_from_deck =
      (fun _ _ ~options -> list_random_element_opt options |> Option.get);
    choose_card_to_copy =
      (fun _ player -> list_random_element_opt player.table |> Option.get);
    choose_cards_to_flip = (fun _ player -> list_random_elements player.table);
    choose_card_to_give =
      (fun _ _ ~options -> list_random_element_opt options |> Option.get);
  }
