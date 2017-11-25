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

let () =
  match Array.to_list Sys.argv with
  | _ :: "-i" :: template :: source :: rewrite_template :: _ ->
    begin match Match.find (to_term template) (to_term source) with
      | Some env ->
        (* Format.printf "Match: %s@." (Environment.to_string env); *)
        let rewritten = Environment.substitute env (to_term rewrite_template) in
        Format.printf "%s" (Printer.to_string rewritten)
      | None -> failwith "No match"
    end
  | _ :: template :: source :: rewrite_template :: _ ->
    let template = In_channel.read_all template in
    let source = In_channel.read_all source in
    let rewrite_template = In_channel.read_all rewrite_template in
    begin match Match.find (to_term template) (to_term source) with
      | Some env ->
        (* Format.printf "Match: %s@." (Environment.to_string env); *)
        let rewritten = Environment.substitute env (to_term rewrite_template) in
        Format.printf "%s" (Printer.to_string rewritten)
      | None -> failwith "No match"
    end
  | _ -> failwith "Unknown arguments"
