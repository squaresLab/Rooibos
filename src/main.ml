open Core
open Rooibos
open Lexing

let pp_position formatter lexbuf =
  let pos = lexbuf.lex_curr_p in
  Format.fprintf formatter "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let to_term s =
  let lexbuf = Lexing.from_string s in
  try Parser.main Lexer.read lexbuf with
  | Parser.Error ->
    failwith (Format.asprintf "%a: syntax error in %s\n" pp_position lexbuf s)

let from_file_exn fn = Fn.compose to_term In_channel.read_all

let () =
  match Array.to_list Sys.argv with
  | _ :: "parse" :: str :: _ ->
    let term = to_term str in
      Format.printf "%s\n" (Term.to_string term)
  | _ :: "-i" :: template :: source :: rewrite_template :: _ ->
    begin match
        Match.all (to_term template) (to_term source)
        |> Sequence.to_list
      with
      | (_, env)::_ ->
        (* Format.printf "Match: %s@." (Environment.to_string env); *)
        let rewritten = Environment.substitute env (to_term rewrite_template) in
        Format.printf "%s" (Printer.to_string rewritten)
      | [] -> failwith "No match"
    end
  | _ :: template :: source :: rewrite_template :: _ ->
    let read = Fn.compose Core.String.rstrip In_channel.read_all in
    let template = read template in
    let source = read source in
    let rewrite_template = read rewrite_template in
    begin match
        Match.all (to_term template) (to_term source)
        |> Sequence.to_list
      with
      | (_, env)::_ ->
        (* Format.printf "Match: %s@." (Environment.to_string env); *)
        let rewritten = Environment.substitute env (to_term rewrite_template) in
        Format.printf "%s" (Printer.to_string rewritten)
      | [] -> failwith "No match"
    end
  | _ -> failwith "Unknown arguments"
