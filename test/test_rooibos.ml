open Core_kernel
open OUnit2
open Lexing

open Rooibos


let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let mockrg = Location.Range.unknown

(* Produces a Location.range.t from a string of the
 * form "line:char#line:char". *)
let rg s =
  let loc_from_s s : Location.t =
    let line, col = match String.split s ~on:':' with
      | line::col::[] ->
        (Int.of_string line), (Int.of_string col)
      | _ -> failwith "illegal string format for location\n"
    in
      Location.create line col
  in
  let start, stop = match String.split s ~on:'#' with
    | start::stop::[] ->
        (loc_from_s start), (loc_from_s stop)
    | _ -> failwith "illegal string format for location range\n"
  in
    Location.Range.create start stop

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

let make_env bindings =
  List.fold bindings
    ~init:(Environment.create ())
    ~f:(fun env (v, term) -> Environment.add env (v,0) term)

let env_of_result template source =
  let _, env = Option.value_exn (Match.find template source) in
  env

let loc_of_result template source =
  let loc, _ = Match.all template source |> Sequence.hd_exn in
  loc

let printer = Environment.to_string

let assert_equiv (e1 : Environment.t) (e2 : Environment.t) =
  let e1, e2 = (Environment.strip e1), (Environment.strip e2) in
    assert_equal ~printer e1 e2


let test_comments _ =
  let (!!) s = !s in
  let (!) s = !!s |> Term.strip in

  !"// hello" |> ignore;
  !"// hello/n//how are you today?" |> ignore;

  assert_equal
    ~printer:Term.to_string_with_loc
    (Comment ("// hello", mockrg))
    (!"// hello");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Comment ("/* hello world */", mockrg))
    (!"/* hello world */");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
               [ Comment ("/* hello */", mockrg)
               ; White (" ", mockrg)
               ; Const ("x", mockrg)
               ; White (" ", mockrg)
               ; Const ("=", mockrg)
               ; White (" ", mockrg)
               ; Const ("y", mockrg)
               ; Const (";", mockrg)
               ; White (" ", mockrg)
               ; Comment ("/* world.\n how are you? */", mockrg)
               ],
               mockrg))
    (!"/* hello */ x = y; /* world.\n how are you? */");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
               [ Comment ("// cool", mockrg)
               ; Break mockrg
               ; Const ("x", mockrg)
               ; White (" ", mockrg)
               ; Const ("=", mockrg)
               ; White (" ", mockrg)
               ; Const ("y", mockrg)
               ; Const (";", mockrg)
               ; Break mockrg
               ; Comment ("// commento", mockrg)],
               mockrg))
    (!"// cool\nx = y;\n// commento")


let test_location _ =
  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Const ("NODELET_ERROR_STREAM", (rg "1:0#1:20"))
              ; Compound ("round",
                         [ Compound ("block",
                                    [ Const ("\"CmdVelMux : yaml parsing problem [\"", (rg "1:21#1:57"))
                                    ; White (" ", (rg "1:57#1:58"))
                                    ; Const ("<<", (rg "1:58#1:60"))
                                    ; White (" ", (rg "1:60#1:61"))
                                    ; Const ("std", (rg "1:61#1:64"))
                                    ; Const (":", (rg "1:64#1:65"))
                                    ; Const (":", (rg "1:65#1:66"))
                                    ; Const ("string", (rg "1:66#1:72"))
                                    ; Compound ("round",
                                               [ Compound ("block",
                                                           [ Const ("e", (rg "1:73#1:74"))
                                                           ; Const (".", (rg "1:74#1:75"))
                                                           ; Const ("what", (rg "1:75#1:79"))
                                                           ; Compound ("round", [], (rg "1:79#1:81")) ],
                                                           (rg "1:73#1:81")) ],
                                               (rg "1:72#1:82"))
                                    ; White (" ", (rg "1:82#1:83"))
                                    ; Const ("+", (rg "1:83#1:84"))
                                    ; White (" ", (rg "1:84#1:85"))
                                    ; Const ("\"]\"", (rg "1:85#1:88"))],
                                    (rg "1:21#1:88")) ],
                         (rg "1:20#1:89"))
              ; Const (";", (rg "1:89#1:90"))],
              (rg "1:0#1:90")))
    (!"NODELET_ERROR_STREAM(\"CmdVelMux : yaml parsing problem [\" << std::string(e.what()) + \"]\");");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Const ("foo", (rg "1:0#1:3"))
              ; Break (rg "1:3#2:0")
              ; Const ("bar", (rg "2:0#2:3"))],
              (rg "1:0#2:3")))
    (!"foo\nbar");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Comment ("// foo", (rg "1:0#1:6"))
              ; Break (rg "1:6#2:0")
              ; Const ("bar", (rg "2:0#2:3"))],
              (rg "1:0#2:3")))
    (!"// foo\nbar");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Comment ("/* foo */", (rg "1:0#1:9"))
              ; Break (rg "1:9#2:0")
              ; Const ("bar", (rg "2:0#2:3"))],
              (rg "1:0#2:3")))
    (!"/* foo */\nbar");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Comment ("/* foo\nbar */", (rg "1:0#2:6"))
              ; Break (rg "2:6#3:0")
              ; Const ("heh", (rg "3:0#3:3"))],
              (rg "1:0#3:3")))
    (!"/* foo\nbar */\nheh");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Const ("x", (rg "1:0#1:1"))
              ; White (" ", (rg "1:1#1:2"))
              ; Const ("=", (rg "1:2#1:3"))
              ; White (" ", (rg "1:3#1:4"))
              ; Const ("\"foo\nbar\nheh\"", (rg "1:4#3:4"))
              ; Const (";", (rg "3:4#3:5"))],
              (rg "1:0#3:5")))
    (!"x = \"foo\nbar\nheh\";");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block",
              [ Const ("x", (rg "1:0#1:1"))
              ; White (" ", (rg "1:1#1:2"))
              ; Const ("=", (rg "1:2#1:3"))
              ; White (" ", (rg "1:3#1:4"))
              ; Const ("'foo\nbar\nheh'", (rg "1:4#3:4"))
              ; Const (";", (rg "3:4#3:5"))],
              (rg "1:0#3:5")))
    (!"x = 'foo\nbar\nheh';");

  assert_equal
    ~printer:Term.to_string_with_loc
    (White (" ", (rg "1:0#1:1")))
    (!" ");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Const ("\"hello world!\"", (rg "1:0#1:14")))
    (!"\"hello world!\"");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block", [Const ("foo",  (rg "1:0#1:3"));
                         White (" ",    (rg "1:3#1:4"));
                         Const ("bar",  (rg "1:4#1:7"))],
               (rg "1:0#1:7")))
    (!"foo bar");

   assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block", [Const ("x", (rg "1:0#1:1"));
                         Var (("1",0), (rg "1:1#1:5"))],
               (rg "1:0#1:5")))
    (!"x:[1]")


let test_match_no_holes _ =
  (* BUG 69 *)
  assert_equal
    ~printer:Location.Range.to_string
    (rg "1:4#1:9")
    (loc_of_result !"a + b" !"foo(a + b + c + d)");

  (* BUG #53 *)
  assert_equal
    ~printer:Location.Range.to_string
    (rg "1:11#1:23")
    (loc_of_result !"print('foo')" !"int x = 4; print('foo');");

  assert_equal
    ~printer:Location.Range.to_string
    (rg "1:82#1:85")
    (loc_of_result !" + " !"NODELET_ERROR_STREAM(\"CmdVelMux : yaml parsing problem [\" << std::string(e.what()) + \"]\");")

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
    ~printer:Term.to_string_with_loc
    (Compound ("block", [Const ("foo",  (rg "1:0#1:3"));
                         White (" ",    (rg "1:3#1:4"));
                         Const ("=",    (rg "1:4#1:5"));
                         White (" ",    (rg "1:5#1:6"));
                         Const ("bar",  (rg "1:6#1:9"));
                         Const (";",    (rg "1:9#1:10"))],
               (rg "1:0#1:10")))
    (!"foo = bar;");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block", [Const  ("x",     (rg "1:0#1:1"));
                         Var    (("1",0), (rg "1:1#1:5"))],
               (rg "1:0#1:5")))
    (!"x:[1]");

  assert_equal
    ~printer:Term.to_string_with_loc
    (Compound ("block", [Const  ("xy",    (rg "1:0#1:2"));
                         Var    (("1",0), (rg "1:2#1:6"))],
               (rg "1:0#1:6")))
    (!"xy:[1]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]");

  assert_raises
    (Exceptions.ParseError  "Please, no consecutive holes allowed")
    (fun () -> !":[_]:[_]:[_]")


let test_strip _ =
  assert_equal
    ~printer:Term.to_string
    (Compound ("block", [Const ("x", (mockrg)); Var (("1",0), (mockrg))], (mockrg)))
    (Term.strip (!"x:[1]"))


(* BUG #58 *)
let test_match_location _ =
  assert_equal
    ~printer:Location.Range.to_string
    (rg "1:4#1:10")
    (loc_of_result !"x = :[1];" !"int x = 4;");

  assert_equal
    ~printer:Location.Range.to_string
    (rg "1:11#1:24")
    (loc_of_result !"print(:[1])" !"int x = 4; print('foo');")


let test_match _ =
  (* BUG #51 *)
  assert_equiv
    (make_env [(("1"), !"\"hello world!\"")])
    (env_of_result !"print(:[1]);" !"print(\"hello world!\");");

  assert_equiv
    (make_env [(("1"), !"foo")])
    (env_of_result !"x = :[1];" !"x = foo; x = bar;");

  assert_equiv
    (make_env [("1", !"a"); ("2", !"b")])
    (env_of_result !"x = :[1] + :[2];" !"x = a + b; x = c + d;");

  assert_equiv
    (make_env [("1", !"y")])
    (env_of_result !"(x(:[1]()))" !"(x(y()))");

  assert_equiv
    (make_env [("1", !"y()")])
    (env_of_result !"(:[1])" !"(y())");

  assert_equiv
    (make_env [("1", !"x = y")])
    (env_of_result !"{ :[1]; }" !"{ x = y; }");

  assert_equiv
    (make_env [("1", !"x"); ("2", !"y()")])
    (env_of_result !"(:[1](:[2]))" !"(x(y()))");

  assert_equiv
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"(:[1](:[2]()))" !"(x(y()))");

  assert_equiv
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"(:[1](:[2]()))" !"(x(y()))");

  assert_equiv
    (make_env [("1", !"(x(y()))")])
    (env_of_result !":[1]" !"(x(y()))");

  assert_equiv
    (make_env [("1", !"x()x")])
    (env_of_result !":[1]" !"x()x");

  assert_equiv
    (make_env [("1", !"z()")])
    (env_of_result !"x(y(:[1]))" !"x(y(z()))");

  assert_equiv
    (make_env [("1", !"()")])
    (env_of_result !"x:[1]x" !"x()x");

  assert_equiv
    (make_env [("1", !"()")])
    (env_of_result !"x:[1]x" !"x()x");

  assert_equiv
    (make_env [("1", !"a,b,c"); ("2", !":")])
    (env_of_result !"x({(:[1])}:[2])x" !"x({(a,b,c)}:)x");

  assert_equiv
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !":[1],:[2]" !"x,y");

  assert_equiv
    (make_env [("1", !"foo[0][1]")])
    (env_of_result !"x = :[1];" !"x = foo[0][1];");

  assert_equiv
    (make_env [("1", !"[0][1]")])
    (env_of_result !"x = foo:[1];" !"x = foo[0][1];");

  assert_equiv
    (make_env [("1", !"x"); ("2", !"y")])
    (env_of_result !"strcpy(:[1],:[2])" !"strcpy(x,y)");

  assert_equiv
    (make_env [("1", !"3"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > 3) { x = y; }");

  assert_equiv
    (make_env [("1", !"f()"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > f()) { x = y; }");

  assert_equiv
    (make_env [("1", !"f()")])
    (env_of_result !"if (x <= :[1] <= 10)" !"if (x <= f() <= 10)");

  assert_equiv
    (make_env [("1", !"f()()"); ("2", !"x = y")])
    (env_of_result !"if (x > :[1]) { :[2]; }" !"if (x > f()()) { x = y; }");

  assert_equiv
    (make_env [("1", !"()()()()")])
    (env_of_result !"{:[1]}" !"{()()()()}");

  assert_equiv
    (make_env [("1", !"()")])
    (env_of_result !"{()()():[1]}" !"{()()()()}");

  assert_equiv
    (make_env [("1", !"()()")])
    (env_of_result !"{()()():[1]}" !"{()()()()()}");

  assert_equiv
    (make_env [("1", !"()()()")])
    (env_of_result !"{():[1]}" !"{()()()()}");

  assert_equiv
    (make_env [("1", !"[{x}[0]]"); ("2", !"{}")])
    (env_of_result !"if (x > f([][:[1]])()) :[2]" !"if (x > f([][[{x}[0]]])()) {}");

  assert_equiv
    (make_env [("1", !".")])
    (env_of_result !"foo:[1]val = 100" !"foo.val = 100");

  assert_equal
    None
    (Match.find !"foo.:[1].val = :[2]" !"foo.val = 100");

  assert_equal
    None
    (Match.find !":[1]();" !"foo(x);");

  assert_equal
    (make_env [("1", !"foo")])
    (env_of_result !":[1]();" !"foo();");

  (* string literals *)

  assert_equiv
    (make_env [("1", !{|"it is wednesday"|})])
    (env_of_result !":[1]" !{|"it is wednesday"|});

  assert_equal
    (make_env [("1", !{|'it is wednesday'|})])
    (env_of_result !":[1]" !{|'it is wednesday'|});

  assert_equiv
    (make_env [("1", !{|"lex"|})])
    (env_of_result !"!:[1]" !{|!"lex"|});

  assert_equal
    (make_env [("1", !"\"lex across\n           newlines\"")])
    (env_of_result !":[1];"
       !{|"lex across
           newlines";|});

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
    let _, env = Option.value_exn (Match.find !template !source) in
    Printer.to_string @@ Environment.substitute env !rewrite_template
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
    |> Sequence.map ~f:Match.environment
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

  let term = !"\"hello world!\"" in
  assert_equal
    ("\"hello world!\"")
    (Printer.to_string term);

  let env =
    let env = Environment.create () in
    Environment.add env ("1",0) (Term.Const (":)", mockrg)) in
  let term =
    !":[1]"
    |> Environment.substitute env in
  assert_equal
    (":)")
    (Printer.to_string term);

  let env =
    let env = Environment.create () in
    Environment.add env ("1",0) (Term.Const ("dst", mockrg)) in
  let term =
    !"strcpy(:[1],src)"
    |> Environment.substitute env in
  assert_equal
    ("strcpy(dst,src)")
    (Printer.to_string term)

  let suite =
    "test" >::: [
    (*
      "test_match_location" >:: test_match_location
    ; "test_location" >:: test_location
    ; "test_comments" >:: test_comments
    ; "test_parser" >:: test_parser
    ; "test_match" >:: test_match
    ; "test_end_to_end" >:: test_end_to_end
    ; "not_handled_tests" >:: not_handled_tests
    ; "test_printer" >:: test_printer
    ; "test_all_match" >:: test_all_match
    ; *) "test_match_no_holes" >:: test_match_no_holes
    ]

let () = run_test_tt_main suite
