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

let () =
  let open Alcotest in
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
        ] );
      ( "Partition list preserves elements",
        [ QCheck_alcotest.to_alcotest partition_list_preserves_elements ] );
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
    ]
