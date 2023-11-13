open Cards
open Player
open Arena

val pp_nigiri : Format.formatter -> nigiri -> unit
val pp_sushi_roll : Format.formatter -> sushi_roll -> unit
val pp_shape : Format.formatter -> shape -> unit
val pp_appetizer : Format.formatter -> appetizer -> unit
val pp_fruit_kind : Format.formatter -> fruit_kind -> unit
val pp_special : Format.formatter -> special -> unit
val pp_dessert : Format.formatter -> dessert -> unit
val pp_facedown : Format.formatter -> unit
val pp_card : Format.formatter -> card -> unit
val pp_titled_card_list : string -> Format.formatter -> card list -> unit
val pp_hand : Format.formatter -> hand -> unit
val pp_player : Format.formatter -> player -> unit
val pp_menu : Format.formatter -> menu -> unit
val pp_win : Format.formatter -> win -> unit
val pp_game_ending : Format.formatter -> game_ending -> unit
val pp_player_status : Format.formatter -> player_status -> unit
val pp_game_status : Format.formatter -> game_status -> unit
