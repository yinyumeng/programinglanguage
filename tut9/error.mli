type error = { error_loc : Globals.loc; error_text : string; }
exception Ppf of (error * int)
val report_error_msg : string -> 'a
val report_error : error -> 'a
val report_no_pattern : unit -> 'a
val report_error1 : error -> string -> 'a
val report_warning : error -> unit
exception Malformed_barrier of string
