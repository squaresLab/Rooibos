open OUnit2

let test_parser ctxt = ()

let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ]

let () = run_test_tt_main suite
