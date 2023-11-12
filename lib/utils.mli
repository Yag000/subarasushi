val partition_list : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Partitions a [list] by a given partition function. *)

val list_remove_index : int -> 'a list -> 'a list
(** Removes the element at the given index from the list. *)

val list_random_element_opt : 'a list -> 'a option
(** Returns a random element from the list. *)

val list_random_elements : 'a list -> 'a list
(** Returns a random set of random size of elements from the list . *)

val list_shuffle : 'a list -> 'a list
(** Shuffles the list. *)
