type 'a t = { term : 'a; loc : Location.Range.t }

(* TODO: add Location.Range.t support *)
let make term = 
  let open Location in
  let loc =
  {
    Range.start_ = { line = 0; column = 0 };
    Range.end_ = { line = 0; column = 0 }
  }
  in
    { term = term; loc = loc }

let range node = node.loc

let term node = node.term

let to_string node = "I WILL BE IMPLEMENTED"
