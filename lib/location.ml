open Core_kernel

type t =
  { offset    : int
  ; line_no   : int
  ; column    : int
  ; file_name : string
  }

let to_string loc =
  loc.file_name ^ ":" ^ (Int.to_string loc.line_no) ^ ":" ^ (Int.to_string loc.column)

let mock =
  { offset = 0
  ; line_no = 0
  ; column = 0
  ; file_name = "foo.c"
  }

module Range = struct
  type k = t 
  type nonrec t =
    { start : t
    ; stop : t
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

let make pos =
  let open Lexing in
    { offset = pos.pos_cnum
    ; line_no = pos.pos_lnum
    ; column = pos.pos_cnum - pos.pos_bol (* TO SET POS_BOL *)
    ; file_name = pos.pos_fname
    }

let offset loc = loc.offset

let line_no loc = loc.line_no

let file_name loc = loc.file_name

let column loc = loc.column
