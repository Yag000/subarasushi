open Player

type points = int

val count_round_points : played_uramakis:int -> player list -> player list
(** Computes the points of a round. It ignores desserts, as they should be counted with the
    function [count_dessert_points]. *)

val count_dessert_points : player list -> player list
(** Computes the points won using the desserts. *)

type position = First | Second | Third | Other | Last

val get_positions : int list -> (int * position) list
val uramaki_points : ('a * position) list -> int -> ('a * points) list
