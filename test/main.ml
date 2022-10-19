open OUnit2
open Game

let suite = "test suite for chess" >::: List.flatten []
let _ = run_test_tt_main suite
