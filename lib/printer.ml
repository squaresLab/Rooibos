open Core
open Format

open Term

let delimiters_of_string = function
  | "round" -> "(", ")"
  | "square" -> "[", "]"
  | "angle" -> "<", ">"
  | "curly" -> "{", "}"
  | "block" -> "", ""
  | s -> failwith @@ sprintf "Unsupported delimiter %s" s


let enclose_with (left,right) s =
  left ^ s ^ right


let rec pp formatter term =
  match term with
  | Const c -> Format.fprintf formatter "%s" c
  | Var (v,_) -> Format.fprintf formatter ":[%s]" v
  | Compound (delimiter, terms) ->
    let left,right = delimiters_of_string delimiter in
    Format.fprintf formatter "%s%a%s" left pp_list terms right
  | Break -> Format.fprintf formatter "@."
  | White w -> Format.fprintf formatter "%s" w
and pp_list formatter term_list =
  match term_list with
  | [] -> Format.fprintf formatter ""
  | [term] -> Format.fprintf formatter "%a" pp term
  | term::terms -> Format.fprintf formatter "%a%a" pp term pp_list terms


let to_string term =
  Format.asprintf "%a" pp term


let pps () term =
  to_string term


let ppo out term : unit =
  let formatter = Format.formatter_of_out_channel out in
  pp formatter term;
  Format.pp_print_flush formatter ()
