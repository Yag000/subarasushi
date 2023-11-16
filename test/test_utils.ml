open Subarasushi.Utils

let partition_list_preserves_elements =
  let open QCheck in
  Test.make ~count:100
    ~name:
      "for all l, for all x in l, x in List.flatten (partition_list ( = ) l)"
      (* This function will test if this statement is true for different values. *)
    (list small_int) (fun l ->
      let p_l = partition_list ( = ) l in
      let l' = List.flatten p_l in
      List.for_all (fun x -> List.mem x l') l)

let random_element_is_always_inside_list =
  let open QCheck in
  Test.make ~count:100 ~name:"for all l, for all x in l, x in random_element l"
    (list small_int) (fun l ->
      let x = list_random_element_opt l in
      match x with None -> l = [] | Some x -> List.mem x l)

let random_sublist_is_included_in_list =
  let open QCheck in
  Test.make ~count:100
    ~name:"for all l, for all x in list_random_elements l, x in l"
    (list small_int) (fun l ->
      let x = list_random_elements l in
      List.for_all (fun x -> List.mem x l) x)

let list_included_in_itself =
  let open QCheck in
  Test.make ~count:100 ~name:"for all l, l in l" (list small_int) (fun l ->
      includes l l)

let sublist_is_included_in_list =
  let open QCheck in
  Test.make ~count:100
    ~name:"for all l1, for all l2 in list_random_elements l1, l2 in l1"
    (list small_int) (fun l1 ->
      let l2 = list_random_elements l1 in
      includes l1 l2)

let () =
  let open Alcotest in
  Random.self_init ();
  run "Utils"
    [
      ( "partition_list",
        [
          test_case "empty_list" `Quick (fun () ->
              let l : int list = [] in
              let p_l = partition_list ( = ) l in
              check (list (list int)) "l1" [] p_l);
          test_case "one_element" `Quick (fun () ->
              let l : int list = [ 1 ] in
              let p_l = partition_list ( = ) l in
              check (list (list int)) "l1" [ [ 1 ] ] p_l);
          test_case "multiple equal elements" `Quick (fun () ->
              let l : int list = [ 1; 1; 1; 1; 1 ] in
              let p_l = partition_list ( = ) l in
              check (list (list int)) "l1" [ [ 1; 1; 1; 1; 1 ] ] p_l);
          test_case "multiple different elements" `Quick (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5 ] in
              let p_l = partition_list ( = ) l in
              check
                (list (list int))
                "l1"
                [ [ 1 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 5 ] ]
                p_l);
          test_case "multiple elements" `Quick (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5; 1; 2; 3; 4; 5 ] in
              let p_l = partition_list ( = ) l in
              check
                (list (list int))
                "l1"
                [ [ 1; 1 ]; [ 2; 2 ]; [ 3; 3 ]; [ 4; 4 ]; [ 5; 5 ] ]
                p_l);
          test_case "non equality equivalence relationship (mod 2)" `Quick
            (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5; 1; 2; 3; 4; 5 ] in
              let p_l =
                partition_list (fun x y -> x mod 2 = y mod 2) l
                (* sort each partition in order to get a reproducible result *)
                |> List.map (List.sort compare)
              in
              check
                (list (list int))
                "l1"
                [ [ 1; 1; 3; 3; 5; 5 ]; [ 2; 2; 4; 4 ] ]
                p_l);
          QCheck_alcotest.to_alcotest partition_list_preserves_elements;
        ] );
      ( "list_remove_index",
        [
          test_case "empty_list" `Quick (fun () ->
              let l : int list = [] in
              let l' = list_remove_index 0 l in
              check (list int) "l1" [] l');
          test_case "one_element" `Quick (fun () ->
              let l : int list = [ 1 ] in
              let l' = list_remove_index 0 l in
              check (list int) "l1" [] l');
          test_case "multiple_elements" `Quick (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5 ] in
              let l' = list_remove_index 0 l in
              check (list int) "l1" [ 2; 3; 4; 5 ] l');
          test_case "multiple_elements" `Quick (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5 ] in
              let l' = list_remove_index 2 l in
              check (list int) "l1" [ 1; 2; 4; 5 ] l');
          test_case "multiple_elements" `Quick (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5 ] in
              let l' = list_remove_index 4 l in
              check (list int) "l1" [ 1; 2; 3; 4 ] l');
          test_case "multiple_elements" `Quick (fun () ->
              let l : int list = [ 1; 2; 3; 4; 5 ] in
              let l' = list_remove_index 10 l in
              check (list int) "l1" [ 1; 2; 3; 4; 5 ] l');
        ] );
      ( "list_random_element_opt",
        [
          test_case "empty_list" `Quick (fun () ->
              let l : int list = [] in
              let l' = list_random_element_opt l in
              check (option int) "l1" None l');
          QCheck_alcotest.to_alcotest random_element_is_always_inside_list;
        ] );
      ( "list_random_elements",
        [
          test_case "empty_list" `Quick (fun () ->
              let l : int list = [] in
              let l' = list_random_elements l in
              check (list int) "l1" [] l');
          QCheck_alcotest.to_alcotest random_sublist_is_included_in_list;
        ] );
      ( "Includes",
        [
          test_case "trivial" `Quick (fun () ->
              let l1 : int list = [] in
              let l2 : int list = [] in
              let b = includes l1 l2 in
              check bool "b" true b);
          test_case "One element included" `Quick (fun () ->
              let l1 : int list = [ 1; 2; 3 ] in
              let l2 : int list = [ 1 ] in
              let b = includes l1 l2 in
              check bool "b" true b);
          QCheck_alcotest.to_alcotest list_included_in_itself;
          QCheck_alcotest.to_alcotest sublist_is_included_in_list;
        ] );
    ]
