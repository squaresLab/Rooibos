type t =
  { line : int
  ; column : int
  }

module Range = struct
  type nonrec t =
    { start_ : t
    ; end_ : t
    }

  let to_string _ = ""
end

let to_string _ = ""
