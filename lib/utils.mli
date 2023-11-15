val partition_list : ('a -> 'a -> bool) -> 'a list -> 'a list list
(** Partitions a [list] by a given partition function. *)

val list_remove_index : int -> 'a list -> 'a list
(** Removes the element at the given index from the list. *)

val list_random_element_opt : 'a list -> 'a option
(** Returns a random element from the list. *)

val list_random_n_elements : int -> 'a list -> 'a list
(** Returns a random set of [n] elements from the list. If [n] is greater than
    the length of the list, the whole list is returned. *)

val list_random_elements : 'a list -> 'a list
(** Returns a random set of random size of elements from the list . *)

val list_shuffle : 'a list -> 'a list
(** Shuffles the list. *)

val find_index : ('a -> bool) -> 'a list -> int option
(** `find_index f xs` returns `Some i`, where `i` is the index of the first element of the list `xs` that satisfies `f x`, if there is such an element.
    It returns `None` if there is no such element. *)

val includes : 'a list -> 'a list -> bool
