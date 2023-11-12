type player = {
  name : string;
  id : int;
  score : int;
  desserts : Cards.dessert list;
  table : Cards.hand;
}
[@@deriving show]

(** Returns a new player with the given name. Every other field is
    initialized to the default value. *)
let default_named_player name =
  { name; id = 0; score = 0; desserts = []; table = [] }
