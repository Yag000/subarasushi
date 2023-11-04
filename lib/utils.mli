val partition_list : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Partitions a [list] by a given partition function. *)

val list_remove_index : int -> 'a list -> 'a list
(** Removes the element at the given index from the list. *)
