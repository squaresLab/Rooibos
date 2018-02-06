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
    ; column  = pos.pos_cnum - pos.pos_bol
    }

let create line column offset =
  { line ; column; offset }

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

  let create start stop =
    { start ; stop }
  let make_loc = make
  let make start stop =
    create (make_loc start) (make_loc stop)

  let start range = range.start
  let stop range = range.stop

  let loc_to_s = to_string
  let to_string range =
    (loc_to_s range.start) ^ "::" ^ (loc_to_s range.stop)

  let mock_loc = mock
  let mock =
    { start = mock_loc
    ; stop  = mock_loc
    }
end
