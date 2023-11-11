type player = {
  name : string;
  id : int;
  score : int;
  desserts : Cards.dessert list;
  table : Cards.hand;
}

val default_named_player : string -> player
(** Returns a new player with the given name. Every other field is
    initialized to the default value. *)
