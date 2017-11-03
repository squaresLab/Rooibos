open Core_kernel
open OUnit2
open Lexing

open Rooibos


let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let (!) s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith @@ Format.asprintf "%a: syntax error in %s\n" pp_position lexbuf s

let assert_fails_with_message message f =
  assert_raises (Failure message) f

let term_to_node term =
  { Node.term; loc = Location.Range.mock }

let n = term_to_node

let cmp { Node.term = term1; _ } { Node.term = term2; _ } =
  Term.compare term1 term2

let test_parser _ =
  let open Term in
  !"" |> ignore;
  !"x" |> ignore;
  !"x()x" |> ignore;
  !"()x()" |> ignore;
  !"{a{[x[z]y]({})}b}d" |> ignore;
  !"(x(:[_]()))" |> ignore;

  assert_fails_with_message
    ":1:2: syntax error in {\n"
    (fun () -> !"{");

  assert_fails_with_message
    ":1:11: syntax error in (x(:[_]())\n"
    (fun () -> !"(x(:[_]())");

  assert_equal
    ~cmp
    ~printer:(fun { Node.term; _ } -> Term.to_string term)
    (n (Compound ("block",[n (Const "x"); n (Var ("1",0))])))
    (!"x:[1]");

  assert_equal
    ~cmp
    (n (Compound ("block",[n (Const "xy"); n (Var ("1",0))])))
    (!"xy:[1]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]:[_]")

let test_match _ =
  let make_env bindings =
    List.fold bindings
      ~init:(Environment.create ())
      ~f:(fun env (v, term) -> Environment.add env (v,0) term)
  in
  let env_of_result template source =
    Option.value_exn (Match.find template source) |> fst in

  assert_equal
    ~printer:Environment.to_string
    (make_env [(("1"), !"foo")])
    (env_of_result !"x = :[1];" !"x = foo; x = bar;");

  assert_equal
    ~printer:Environment.to_string
    (make_env [("1", !"a"); ("2", !"b")])
    (env_of_result !"x = :[1] + :[2];" !"x = a + b; x = c + d;")

let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ; "test_match" >:: test_match
  ]

let () = run_test_tt_main suite
