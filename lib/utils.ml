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

(** Returns a random element from the list. *)
let list_random_element_opt l =
  match l with [] -> None | _ -> Random.int (List.length l) |> List.nth_opt l

let choose_aux l =
  let index = List.length l |> Random.int in
  (List.nth l index, index)

(** Returns a random set of random size of elements from the list . *)
let list_random_elements l =
  let len = List.length l in
  if len = 0 then []
  else
    let n = Random.int len in
    List.init n (fun _ -> ())
    |> List.fold_left
         (fun (result, l) _ ->
           let e, i = choose_aux l in
           (e :: result, list_remove_index i l))
         ([], l)
    |> fst

(** Shuffles the list. *)
let list_shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  List.sort compare nd |> List.map snd
