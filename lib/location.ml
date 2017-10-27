open Core_kernel

type t =
  { offset    : int
  ; line_no   : int
  ; column    : int
  ; file_name : string
  }

let to_string loc =
  loc.file_name ^ ":" ^ (Int.to_string loc.line_no) ^ ":" ^ (Int.to_string loc.offset)

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
end

let make pos =
  let open Lexing in
    { offset = pos.pos_cnum
    ; line_no = pos.pos_lnum
    ; column = (pos.pos_cnum - pos.pos_bol + 1)
    ; file_name = pos.pos_fname
    }

let offset loc = loc.offset

let line_no loc = loc.line_no

let file_name loc = loc.file_name

let column loc = loc.column