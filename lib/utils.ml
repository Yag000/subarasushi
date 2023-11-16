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

(** Returns a random set of [n] elements from the list. If [n] is greater than
    the length of the list, the whole list is returned. *)
let list_random_n_elements n l =
  let len = List.length l in
  if len = 0 || n = 0 then []
  else if n >= len then l
  else
    List.init n (fun _ -> ())
    |> List.fold_left
         (fun (result, l) _ ->
           let e, i = choose_aux l in
           (e :: result, list_remove_index i l))
         ([], l)
    |> fst

(** Returns a random set of random size of elements from the list . *)
let list_random_elements l =
  let len = List.length l in
  if len = 0 then []
  else
    let n = Random.int len in
    list_random_n_elements n l

(** Shuffles the list. *)
let list_shuffle l =
  let nd = List.map (fun c -> (Random.bits (), c)) l in
  List.sort compare nd |> List.map snd

(** `find_index f xs` returns `Some i`, where `i` is the index of the first element of the list `xs` that satisfies `f x`, if there is such an element.
    It returns `None` if there is no such element. *)
let find_index f =
  let rec fi index = function
    | [] -> None
    | x :: xs -> if f x then Some index else fi (index + 1) xs
  in
  fi 0

let is_empty l = List.length l = 0

(** Partition a [('a * 'b) list] using a predicate ['b -> bool] 
    @see List.partition *)
let partition_by_snd f = List.partition (fun (_, snd) -> f snd)

(** A c B *)
let includes setA setB =
  List.fold_left
    (fun (set, b) x ->
      if not b then (set, b)
      else if set = [] then (set, false)
      else
        match find_index (( = ) x) set with
        | None -> (set, false)
        | Some i -> (list_remove_index i set, b))
    (setA, true) setB
  |> snd
