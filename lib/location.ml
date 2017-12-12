open Core_kernel

type t =
  { line : int
  ; column : int
  ; offset : int
  }

let to_string loc =
  (Int.to_string loc.line) ^ ":" ^ (Int.to_string loc.column)

module Range = struct
  type nonrec t =
    { start : t
    ; stop : t
    }

  let loc_to_s = to_string
  let to_string range =
    (loc_to_s range.start) ^ "::" ^ (loc_to_s range.stop)
end

