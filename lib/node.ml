type 'a t = { term : 'a; loc : Location.Range.t }

let make term loc = 
  { term = term; loc = loc }

let range node = node.loc

let term node = node.term

let to_string node = "I WILL BE IMPLEMENTED"
