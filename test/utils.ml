open Subarasushi.Cards

let deck =
  Alcotest.testable (Fmt.of_to_string (Format.asprintf "%a" pp_deck)) equal_deck
