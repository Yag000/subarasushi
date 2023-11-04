(** Partitions a [list] by a given partition function. *)
let partition_list partition_function l =
  let rec aux card partition =
    match partition with
    | [] -> [ [ card ] ]
    | x :: xs ->
        let first_card = List.hd x in
        if partition_function card first_card then (card :: x) :: xs
        else x :: aux card xs
  in
  List.fold_left (fun acc x -> aux x acc) [] l

(** Removes the element at the given index from the list. *)
let list_remove_index index l = List.filteri (fun i _ -> i <> index) l
