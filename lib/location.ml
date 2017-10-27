type t =
  { offset : int }

module Range = struct
  type nonrec t =
    { start_ : t
    ; end_ : t
    }

  let make start stop =
    let start = { offset = start } in
    let stop = { offset = stop } in
    { start_ = start; end_ = stop }

  let to_string _ = ""
end

let to_string _ = ""
