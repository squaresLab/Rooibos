open Core_kernel
open OUnit2
open Lexing

open Rooibos


let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

(* converts a node into a sequence of locations *)
let rec node_to_locs (node : Term.t Node.t) : Location.t list =
  let open Term in
  let range = Node.range node in
  let start r = Location.Range.start r in
  let stop r = Location.Range.stop r in
  match Node.term node with
  | Break ->
      [start range]
  | Const _ ->
      [start range; stop range]
  | Var _ ->
      [start range; stop range]
  | Compound (_, l) ->
    let mid = List.concat (List.map ~f:node_to_locs l) in
      [start range] @ mid @ [stop range]

(* DANGER: copy pasto *)
let format s =
  let s = s |> String.chop_prefix_exn ~prefix:"\n"
  in
  let leading_indentation =
    Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' '))
  in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"

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

let test_location _ =
  let open Location.Range in
  (* converts loc to a string of the form "line:col" *)
  let l loc =
    Printf.sprintf "%d:%d" (Location.line_no loc) (Location.column loc)
  in

  let node = !"x = y + z;" in
  let { start; stop } = Node.range node in
    assert_equal ~printer:ident "1:1" (l start);
    assert_equal ~printer:ident "1:10" (l stop)

(*
 let locs = node_to_locs node in
  let expected =
    [(0, 0); (2, 0); (* if *)
     (3, 0); (* ( *)
     (4, 0); (4, 0); (* x *)
     (6, 0); (6, 0); (* < *)
     (8, 0); (8, 0); (* 3 *)
     (9, 0); (9, 0); (* ) *)
     (11, 0); (11, 0); (* { *)
     (2, 1); (6, 1); (* exit *)
     (7, 1); (7, 1); (* ( *)
     (8, 1); (8, 1); (* 0 *)
     (9, 1); (9, 1); (* ) *)
     (10, 1); (10, 1); (* ; *)
     (0, 2); (* } *)
    ]
  in
*)


let suite =
  "test" >::: [
    "test_parser" >:: test_parser
  ; "test_match" >:: test_match
  ; "test_location" >:: test_location
  ]

let () = run_test_tt_main suite
