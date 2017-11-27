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

let format s =
  let s = s |> String.chop_prefix_exn ~prefix:"\n" in
  let leading_indentation =
    Option.value_exn (String.lfindi s ~f:(fun _ c -> c <> ' ')) in
  s
  |> String.split ~on:'\n'
  |> List.map ~f:(Fn.flip String.drop_prefix leading_indentation)
  |> String.concat ~sep:"\n"
  |> String.chop_suffix_exn ~suffix:"\n"

let assert_fails_with_message message f =
  assert_raises (Failure message) f

let test_parser _ =
  !"" |> ignore;
  !"x" |> ignore;
  !"x()x" |> ignore;
  !"()x()" |> ignore;
  !"{a{[x[z]y]({})}b}d" |> ignore;
  !"(x(:[_]()))" |> ignore;
  !"if (x < y) {}" |> ignore;

  assert_fails_with_message
    ":1:2: syntax error in {\n"
    (fun () -> !"{");

  assert_fails_with_message
    ":1:11: syntax error in (x(:[_]())\n"
    (fun () -> !"(x(:[_]())");

  assert_equal
    ~printer:Term.to_string
    (Compound ("block",[Const "x"; Var ("1",0)]))
    (!"x:[1]");

  assert_equal
    ~printer:Term.to_string
    (Compound ("block",[Const "xy"; Var ("1",0)]))
    (!"xy:[1]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]:[_]")



let make_env bindings =
  List.fold bindings
    ~init:(Environment.create ())
    ~f:(fun env (v, term) -> Environment.add env (v,0) term)

let env_of_result template source =
  Option.value_exn (Match.find template source)

let printer = Environment.to_string

let test_match _ =
  assert_equal
    ~printer
    (make_env [(("1"), !"foo")])
    (env_of_result !"x = :[1];" !"x = foo; x = bar;");

  assert_equal
    ~printer
    (make_env [("1", !"a"); ("2", !"b")])
    (env_of_result !"x = :[1] + :[2];" !"x = a + b; x = c + d;");

  assert_equal
    (make_env [("1", !"y")])
    (env_of_result !"(x(:[1]()))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"y()")])
    (env_of_result !"(:[1])" !"(y())");

  assert_equal
    ~printer:Environment.to_string
    (make_env [("1", !"x = y")])
    (env_of_result !"{ :[1]; }" !"{ x = y; }");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y()")])
    (env_of_result !"(:[1](:[2]))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"(:[1](:[2]()))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"(:[1](:[2]()))" !"(x(y()))");

  assert_equal
    (make_env [("1", !"(x(y()))")])
    (env_of_result !":[1]" !"(x(y()))");

  assert_equal
    (make_env [("1", !"x()x")])
    (env_of_result !":[1]" !"x()x");

  assert_equal
    (make_env [("1", !"z()")])
    (env_of_result !"x(y(:[1]))" !"x(y(z()))");

  assert_equal
    (make_env [("1", !"()")])
    (env_of_result !"x:[1]x" !"x()x");

  assert_equal
    (make_env [("1", !"()")])
    (env_of_result !"x:[1]x" !"x()x");

  assert_equal
    (make_env [("1", !"a,b,c"); ("2", !":")])
    (env_of_result !"x({(:[1])}:[2])x" !"x({(a,b,c)}:)x");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !":[1],:[2]" !"x,y");

  assert_equal
    (make_env [("1", !"foo[0][1]")])
    (env_of_result !"x = :[1];" !"x = foo[0][1];");

  assert_equal
    (make_env [("1", !"[0][1]")])
    (env_of_result !"x = foo:[1];" !"x = foo[0][1];");

  assert_equal
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"strcpy(:[1],:[2])" !"strcpy(x,y)");

  assert_equal
    (make_env [("1", !"3"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > 3) { x = y; }");

  assert_equal
    (make_env [("1", !"f()"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > f()) { x = y; }");

  assert_equal
    (make_env [("1", !"f()")])
    (env_of_result !"if (x <= :[1] <= 10)" !"if (x <= f() <= 10)");

  assert_equal
    (make_env [("1", !"f()()"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > f()()) { x = y; }");

  assert_equal
    (make_env [("1", !"()()()()")])
    (env_of_result !"{:[1]}" !"{()()()()}");

  assert_equal
    (make_env [("1", !"()")])
    (env_of_result !"{()()():[1]}" !"{()()()()}");

  assert_equal
    (make_env [("1", !"()()")])
    (env_of_result !"{()()():[1]}" !"{()()()()()}");

  assert_equal
    (make_env [("1", !"()()()")])
    (env_of_result !"{():[1]}" !"{()()()()}");

  assert_equal
    (make_env [("1", !"[{x}[0]]"); ("2", !"{}")])
    (env_of_result !"if (x > f([][:[1]])()) :[2]" !"if (x > f([][[{x}[0]]])()) {}");

  assert_equal
    (make_env [("1", !".")])
    (env_of_result !"foo:[1]val = 100" !"foo.val = 100");

  assert_equal
    None
    (Match.find !"foo.:[1].val = :[2]" !"foo.val = 100");

  (* string literals *)

  assert_equal
    (make_env [("1", !{|"it is wednesday"|})])
    (env_of_result !":[1]" !{|"it is wednesday"|});

  assert_equal
    (make_env [("1", !{|'it is wednesday'|})])
    (env_of_result !":[1]" !{|'it is wednesday'|});

  assert_equal
    (make_env [("1", !{|printf("\033[38;5;0m");|})])
    (env_of_result !":[1]" !{|printf("\033[38;5;0m");|});

  assert_equal
    (make_env [("1", !{|"unbalanced [ ( { legal in string literal"|})])
    (env_of_result !":[1]" !{|"unbalanced [ ( { legal in string literal"|});

  assert_fails_with_message
    "String is not terminated"
    (fun () -> !{| unterminated string " literal |});

  assert_fails_with_message
    "String is not terminated"
    (fun () -> !{| unterminated string ' literal |});

  (* no match support inside string literals for now *)

  assert_equal
    None
    (Match.find !{|"prefix :[1] suffix"|} !{|"prefix x suffix"|})


let test_end_to_end _ =
  let rewrite template source rewrite_template =
    Option.value_exn (Match.find !template !source)
    |> Fn.flip Environment.substitute !rewrite_template
    |> Printer.to_string
  in

  let template =
    {|
      strcpy(:[1],:[2]);
    |}
  in

  let source =
    {|
      strcpy(dst,src);
    |}
  in

  let rewrite_template =
    {|
      strncpy(:[1],:[2],5);
    |}
  in

  assert_equal
    {|
      strncpy(dst,src,5);
    |}
    (rewrite template source rewrite_template);

  (* adds whitespace in template *)
  let template =
    {|
      strcpy(    :[1], :[2])   ;
    |}
  in

  assert_equal
    {|
      strncpy(dst,src,5);
    |}
    (rewrite template source rewrite_template);

  (* adds whitespace in rewrite template *)
  let rewrite_template =
    {|
      strncpy(:[1],   :[2],  5);
    |}
  in

  assert_equal
    ~printer:ident
    {|
      strncpy(dst,   src,  5);
    |}
    (rewrite template source rewrite_template);

  (* adds whitespace in source *)
  let source =
    {|
      strcpy  (  dst, src)   ;
    |}
  in

  assert_equal
    ~printer:ident
    {|
      strncpy(dst,   src,  5);
    |}
    (rewrite template source rewrite_template)


let test_all_match _ =
  let rewrite template source rewrite_template =
    Match.all !template !source
    |> Sequence.map ~f:(Fn.flip Environment.substitute !rewrite_template)
    |> Sequence.map ~f:Printer.to_string
    |> Sequence.to_list
    |> String.concat ~sep:" AND "
  in

  (* Multi-line case *)

  let source =
    {|
      assert(stream->md_len + md_len -
             si.main_data_begin <= MAD_BUFFER_MDLEN);

      memcpy(*stream->main_data + stream->md_len,
             mad_bit_nextbyte(&stream->ptr),
             frame_used = md_len - si.main_data_begin);
      stream->md_len += frame_used;
    |}
  in

  let template =
    {|
      memcpy(:[1], :[2], :[3]);
    |}
  in

  let rewrite_template =
    {|
      ||:[1]||:[2]||:[3]||
    |}
  in

  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    ({|
      ||*stream->main_data + stream->md_len||
             mad_bit_nextbyte(&stream->ptr)||
             frame_used = md_len - si.main_data_begin||
    |} |> format)
    (rewrite template source rewrite_template |> format);

  (* Multi-match to multi-environment case *)

  let source =
    {|
      memcpy(dst1, src1, 1);

      // blah blah

      memcpy(dst2, src2, 2);
    |}
  in

  let template =
    {|
      memcpy(:[1], :[2], :[3]);
    |}
  in

  let rewrite_template =
    {|||:[1]||:[2]||:[3]|||}
  in

  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    ({|
      ||dst2||src2||2|| AND ||dst1||src1||1||
    |} |> format)
    (rewrite template source rewrite_template);

  (* All matching inside nested terms *)

  let source =
    {|
      {
        strcpy(conf.local.path, (yyvsp[0].string));
      }
    |}
  in

  let template =
    {|
      strcpy(:[1], :[2]);
    |}
  in

  let rewrite_template =
    {|
      :[1]||:[2]
    |}
  in

  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    {|
      conf.local.path||(yyvsp[0].string)
    |}
    (rewrite template source rewrite_template);

  let source =
    {|
      {
      {}
      {{[]{ ()
        strcpy(conf.local.path, (yyvsp[0].string));
      }}}
      }
    |}
  in
  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    {|
      conf.local.path||(yyvsp[0].string)
    |}
    (rewrite template source rewrite_template);

  let source =
    {|
      {
      {}
      {{[]{ ()
        strcpy(conf.local.path, (yyvsp[0].string));
      }}}
      }
    |}
  in
  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    {|
      conf.local.path||(yyvsp[0].string)
    |}
    (rewrite template source rewrite_template);

  (* Multi nested matching *)

  let source =
    {|
      x x y strcpy(strcpy(dst1,src1),src2); blah blah XXX
    |}
  in

  (* needs format because prefix and suffix whitespace is match-sensitive *)
  let template =
    {|
      strcpy(:[1], :[2])
    |} |> format
  in

  let rewrite_template =
    {|
      :[1]||:[2]
    |} |> format
  in

  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    {|dst1||src1 AND strcpy(dst1,src1)||src2|}
    (rewrite template source rewrite_template);

  (* multiple match without semicolon *)

  let source =
    {|
      foo(bar, quux);~~~ foo(foobar, bazz)
    |}
  in

  let template =
    {|
      foo(:[1], :[2])
    |} |> format
  in

  let rewrite_template =
    {|
      :[1]||:[2]
    |} |> format
  in

  assert_equal
    ~printer:(fun s ->
        String.concat_map s ~sep:"," ~f:Char.to_string)
    {|foobar||bazz AND bar||quux|}
    (rewrite template source rewrite_template);

  ()


let not_handled_tests _ =
  (* this case must still be handled: match hole to empty string *)
  let _' _ =
    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"")])
      (env_of_result !"foo.:[1]val = 100" !"foo.val = 100")
  in

  (* this case does not match because we don't split up characters *)
  let _' _ =
    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"a")])
      (env_of_result !":[1]a" !"aa")
  in

  (* this test case passes if we treat newlines and white space the same.
     without it, we need to do even more work in matching and lookahead
     another character, because we can get this:

     Matching Sz 6 N_debug(H(1), CR, W(" "), C(<=), W(" "), C(10)) With Sz 5
     N_debug(CR, W(" "), C(<=), W(" "), C(10))

     Where 'CR' is not considered a suffix, and rightly so: we want to
     match across newlines. But the character after CR, W, is also
     not a suffix, and so it doesn't match.
  *)
  let _' _ =
    assert_equal
      (make_env [("1", !"f()")])
      (env_of_result !"if    (x <= :[1]\n <= 10)" !"if (x <= f()\n <= 10)")
  in

  (* this case does not match because we are matching:
     <C(H), C(a)> with
     <C(a), C(a)

     This hits the case where the suffix of C(H), C(a), matches the
     thing we are matching against (non-greedy), and stops matching.
     We are looking ahead one only.

     Question: would it be safe to
     continue sliding the source until we see the last C(a)?
     Answer: No. In general, we do not know when to stop without
     infinite look ahead. Consider this case:

     "a a :[1] a a a" to match with
     "a a a a  a a a"

     Now :[1] needs to match with "a a", but we would not know to collect up
     to two a's before the common suffixes match, becuase to kno that we need
     to look three characters ahead (not one). In the worst case we need to
     look ahead infinitely. In general, we should decide whether to just not
     bother with this case. *)

  let _' _ =
    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"a")])
      (env_of_result !":[1] a" !"a a");

    assert_equal
      ~printer:Environment.to_string
      (make_env [("1", !"a a")])
      (env_of_result !":a [1] a" !"a a")
  in
  ()

let test_printer _ =
  let term = !"x()" in
  assert_equal
    ("x()")
    (Printer.to_string term);

  let term = !":[1]" in
  assert_equal
    (":[1]")
    (Printer.to_string term);

  let env =
    let env = Environment.create () in
    Environment.add env ("1",0) (Term.Const ":)") in
  let term =
    !":[1]"
    |> Environment.substitute env in
  assert_equal
    (":)")
    (Printer.to_string term);

  let env =
    let env = Environment.create () in
    Environment.add env ("1",0) (Term.Const "dst") in
  let term =
    !"strcpy(:[1],src)"
    |> Environment.substitute env in
  assert_equal
    ("strcpy(dst,src)")
    (Printer.to_string term)

  let suite =
    "test" >::: [
      "test_parser" >:: test_parser
    ; "test_match" >:: test_match
    ; "test_end_to_end" >:: test_end_to_end
    ; "not_handled_tests" >:: not_handled_tests
    ; "test_printer" >:: test_printer
    ; "test_all_match" >:: test_all_match
    ]

let () = run_test_tt_main suite
