open Core_kernel

type t =
  { line : int
  ; column : int
  ; offset : int
  }

let to_string loc =
  (Int.to_string loc.line) ^ ":" ^ (Int.to_string loc.column)

let make pos =
  let open Lexing in
    { offset  = pos.pos_cnum
    ; line    = pos.pos_lnum
    ; column  = pos.pos_cnum - pos.pos_bol (* TO SET POS_BOL *)
    }

let mock =
  { offset = 0
  ; line = 0
  ; column = 0
  }

module Range = struct
  type k = t
  type t =
    { start : k
    ; stop : k
    }

  let make start stop =
    { start = start; stop = stop }

  let start range = range.start
  let stop range = range.stop

  let loc_to_s = to_string
  let to_string range =
    (loc_to_s range.start) ^ "::" ^ (loc_to_s range.stop)

  let mock_loc = mock
  let mock = make mock_loc mock_loc
end
