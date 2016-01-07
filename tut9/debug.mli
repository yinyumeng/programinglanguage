val debug_on : bool ref
val devel_debug_on : bool ref
val devel_debug_print_orig_conseq : bool ref
val trace_on : bool ref
val log_devel_debug : bool ref
val debug_log : Buffer.t
val clear_debug_log : unit -> unit
val get_debug_log : unit -> string
val enable_dd_and_orig_conseq_printing : unit -> unit
val string_of_pos : Globals.loc -> string
val print : string -> unit
val pprint : string -> Globals.loc -> unit
val ho_print : bool -> ('a -> string) -> 'a -> unit
val devel_print : string -> unit
val prior_msg : Globals.loc -> string
val devel_pprint : string -> Globals.loc -> unit
val devel_hprint : ('a -> string) -> 'a -> Globals.loc -> unit
val devel_zprint : string Lazy.t -> Globals.loc -> unit
val dinfo_zprint : string Lazy.t -> Globals.loc -> unit
val dinfo_hprint : ('a -> string) -> 'a -> Globals.loc -> unit
val dinfo_pprint : string -> Globals.loc -> unit
val binfo_pprint : string -> Globals.loc -> unit
val binfo_hprint : ('a -> string) -> 'a -> Globals.loc -> unit
val binfo_zprint : string Lazy.t -> Globals.loc -> unit
val binfo_start : string -> unit
val binfo_end : string -> unit
val dinfo_start : string -> unit
val dinfo_end : string -> unit
val ninfo_zprint : 'a -> 'b -> unit
val ninfo_hprint : 'a -> 'b -> 'c -> unit
val ninfo_pprint : 'a -> 'b -> unit
val add_str : string -> ('a -> string) -> 'a -> string
val gen_vv_flags : int -> bool * string
val verbose_hprint : int -> ('a -> string) -> 'a -> unit
val vv_pprint : int -> string -> unit
val vv_hprint : int -> ('a -> string) -> 'a -> unit
val vv_zprint : int -> string Lazy.t -> unit
val vv_plist : int -> (string * string) list -> unit
val vv_hdebug : ('a -> string) -> 'a -> unit
val vv_pdebug : string -> unit
val vv_debug : string -> unit
val vv_trace : string -> unit
val vv_zdebug : string Lazy.t -> unit
val vv_result : string -> int -> (string * string) list -> unit
val trace_pprint : string -> Globals.loc -> unit
val trace_hprint : ('a -> string) -> 'a -> Globals.loc -> unit
val trace_zprint : string Lazy.t -> Globals.loc -> unit
val tinfo_zprint : string Lazy.t -> Globals.loc -> unit
val tinfo_hprint : ('a -> string) -> 'a -> Globals.loc -> unit
val tinfo_pprint : string -> Globals.loc -> unit
val info_pprint : string -> Globals.loc -> unit
val info_hprint : ('a -> string) -> 'a -> Globals.loc -> unit
val info_zprint : string Lazy.t -> Globals.loc -> unit
val print_info : string -> string -> Globals.loc -> unit
val pick_front : int -> string list -> string list
module DebugCore :
  sig
    val ho_aux :
      bool ->
      (int * string Lazy.t) list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'z) option ->
      string -> string list -> ('z -> string) -> ('a -> 'z) -> 'a -> 'z
    val choose : bool list -> ('a * 'b) list -> ('a * 'b) list
    val ho_aux_no : ('a -> 'z) -> 'a -> 'z
    val ho_1_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'z) option ->
      string -> ('a -> string) -> ('z -> string) -> ('a -> 'z) -> 'a -> 'z
    val ho_2_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'b -> 'z) option ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('z -> string) -> ('a -> 'b -> 'z) -> 'a -> 'b -> 'z
    val ho_3_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'b -> 'c -> 'z) option ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('z -> string) -> ('a -> 'b -> 'c -> 'z) -> 'a -> 'b -> 'c -> 'z
    val ho_4_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'z) option ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('z -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'z) -> 'a -> 'b -> 'c -> 'd -> 'z
    val ho_5_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'z) option ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('z -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'z) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'z
    val ho_6_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z) option ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('z -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z
    val ho_7_opt_aux :
      bool ->
      bool list ->
      bool ->
      ('z -> bool) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h -> 'z) option ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('h -> string) ->
      ('z -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h -> 'z) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h -> 'z
    val ho_1_preopt :
      ('a -> bool) ->
      bool ->
      string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val to_1_preopt :
      ('a -> bool) ->
      bool ->
      string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val ho_1_pre :
      bool ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val to_1_pre :
      bool ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val to_1 :
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val ho_1_loop :
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    type debug_option = DO_None | DO_Trace | DO_Loop | DO_Both | DO_Normal
    val string_of_debug_option : debug_option -> string
    val debug_map : (string, debug_option) Hashtbl.t
    val regexp_line : (Str.regexp * debug_option) list ref
    val regexp_line_str : string list ref
    val z_debug_file : string ref
    val z_debug_flag : bool ref
    val debug_file : unit -> in_channel option
    val read_from_debug_file : in_channel -> string list
    val proc_option : String.t list -> bool * bool
    val get_words : string -> string list
    val add_entry_with_options :
      (string -> debug_option -> unit) -> string list -> unit
    val read_main : unit -> unit
    val in_debug : string -> debug_option
    val go_1 :
      bool ->
      bool ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val go_2 :
      bool ->
      bool ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val go_3 :
      bool ->
      bool ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val go_4 :
      bool ->
      bool ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val go_5 :
      bool ->
      bool ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val go_6 :
      bool ->
      bool ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val go_7 :
      bool ->
      bool ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('h -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
    val ho_1 :
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val ho_2 :
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val ho_3 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val ho_4 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val ho_5 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val ho_6 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val ho_7 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('h -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
    val splitter : string -> 'a -> ('b -> 'a) -> (bool -> bool -> 'b) -> 'a
    val no_1 :
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val no_2 :
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val no_3 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val no_4 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val no_5 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val no_6 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val no_7 :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('h -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
    val ho_1_opt :
      bool ->
      bool ->
      ('a -> bool) ->
      string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val ho_2_opt :
      bool ->
      bool ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val ho_3_opt :
      bool ->
      bool ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val ho_4_opt :
      bool ->
      bool ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val ho_5_opt :
      bool ->
      bool ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val ho_6_opt :
      bool ->
      bool ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
    val no_1_opt :
      ('a -> bool) ->
      string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val no_2_opt :
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val no_3_opt :
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val no_4_opt :
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val no_5_opt :
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val no_6_opt :
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
    val add_num : (string -> 'a) -> int -> string -> 'a
    val go_1_num :
      bool ->
      bool ->
      int ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val go_2_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val go_3_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val go_4_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val go_5_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val go_6_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val add_num_opt : ('a -> string -> 'b) -> int -> 'a -> string -> 'b
    val go_1_num_opt :
      bool ->
      bool ->
      int ->
      ('a -> bool) ->
      string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val go_2_num_opt :
      bool ->
      bool ->
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val go_3_num_opt :
      bool ->
      bool ->
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val go_4_num_opt :
      bool ->
      bool ->
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val go_5_num_opt :
      bool ->
      bool ->
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val go_6_num_opt :
      bool ->
      bool ->
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
    val to_1_loop :
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val to_2_loop :
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val to_3_loop :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val to_4_loop :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val to_5_loop :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val to_6_loop :
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val to_1_loop_num :
      int ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val to_2_loop_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val to_3_loop_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val to_4_loop_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val to_5_loop_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val to_6_loop_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val no_1_num_opt :
      int ->
      ('a -> bool) ->
      string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val no_2_num_opt :
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val no_3_num_opt :
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val no_4_num_opt :
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val no_5_num_opt :
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val no_6_num_opt :
      int ->
      ('a -> bool) ->
      string ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
    val no_1_num :
      int ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val no_2_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val no_3_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val no_4_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val no_5_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val no_6_num :
      int ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val ho_1_cmp :
      bool ->
      bool ->
      ('a -> 'b) ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val ho_2_cmp :
      bool ->
      bool ->
      ('a -> 'b -> 'c) ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val ho_3_cmp :
      bool ->
      bool ->
      ('a -> 'b -> 'c -> 'd) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val ho_4_cmp :
      bool ->
      bool ->
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val ho_5_cmp :
      bool ->
      bool ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val ho_6_cmp :
      bool ->
      bool ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val no_1_cmp :
      ('a -> 'b) ->
      string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val no_2_cmp :
      ('a -> 'b -> 'c) ->
      string ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val no_3_cmp :
      ('a -> 'b -> 'c -> 'd) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val no_4_cmp :
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val no_5_cmp :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val no_6_cmp :
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      string ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val ho_eff_1 :
      bool ->
      bool ->
      string ->
      bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val ho_eff_2 :
      bool ->
      bool ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val ho_eff_3 :
      bool ->
      bool ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val ho_eff_4 :
      bool ->
      bool ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val ho_eff_5 :
      bool ->
      bool ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val ho_eff_6 :
      bool ->
      bool ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val no_eff_1 :
      string ->
      bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val no_eff_2 :
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val no_eff_3 :
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val no_eff_4 :
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val no_eff_5 :
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val no_eff_6 :
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val ho_eff_1_num :
      bool ->
      bool ->
      int ->
      string ->
      bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val ho_eff_2_num :
      bool ->
      bool ->
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val ho_eff_3_num :
      bool ->
      bool ->
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val ho_eff_4_num :
      bool ->
      bool ->
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val ho_eff_5_num :
      bool ->
      bool ->
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val ho_eff_6_num :
      bool ->
      bool ->
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val no_eff_1_num :
      int ->
      string ->
      bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
    val no_eff_2_num :
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val no_eff_3_num :
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val no_eff_4_num :
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val no_eff_5_num :
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val no_eff_6_num :
      int ->
      string ->
      bool list ->
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val ho_1_all :
      bool ->
      bool ->
      string ->
      ('a -> bool) ->
      ('b -> 'a) option ->
      bool list -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val ho_2_all :
      bool ->
      bool ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val ho_3_all :
      bool ->
      bool ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val ho_4_all :
      bool ->
      bool ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'e -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val ho_5_all :
      bool ->
      bool ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val ho_6_all :
      bool ->
      bool ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
    val ho_1_all_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> bool) ->
      ('b -> 'a) option ->
      bool list -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val ho_2_all_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val ho_3_all_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val ho_4_all_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'e -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val ho_5_all_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val ho_6_all_num :
      bool ->
      bool ->
      int ->
      string ->
      ('a -> bool) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
    val no_1_all :
      int ->
      string ->
      ('a -> bool) option ->
      ('b -> 'a) option ->
      bool list -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
    val no_2_all :
      int ->
      string ->
      ('a -> bool) option ->
      ('b -> 'c -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
    val no_3_all :
      int ->
      string ->
      ('a -> bool) option ->
      ('b -> 'c -> 'd -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
    val no_4_all :
      int ->
      string ->
      ('a -> bool) option ->
      ('b -> 'c -> 'd -> 'e -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
    val no_5_all :
      int ->
      string ->
      ('a -> bool) option ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
    val no_6_all :
      int ->
      string ->
      ('a -> bool) option ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) option ->
      bool list ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) ->
      ('a -> string) ->
      ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
      'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
  end
module DebugEmpty :
  sig
    val read_main : unit -> unit
    val no_1 : 'a -> 'b -> 'c -> 'd -> 'd
    val no_2 : 'a -> 'b -> 'c -> 'd -> 'e -> 'e
    val no_3 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_4 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_5 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_6 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_7 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'j
    val no_1_cmp : 'a -> 'b -> 'c -> 'd -> 'e -> 'e
    val no_2_cmp : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_3_cmp : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_4_cmp : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_5_cmp : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_6_cmp :
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'j
    val no_1_opt : 'a -> 'b -> 'c -> 'd -> 'e -> 'e
    val no_2_opt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_3_opt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_4_opt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_5_opt : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_6_opt :
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'j
    val no_eff_1 : 'a -> 'b -> 'c -> 'd -> 'e -> 'e
    val no_eff_2 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_eff_3 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_eff_4 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_eff_5 : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_eff_6 :
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'j
    val no_eff_1_num : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_eff_2_num : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_eff_3_num : 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_eff_4_num :
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_eff_5_num :
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'j
    val no_eff_6_num :
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'k -> 'k
    val no_1_num : int -> 'a -> 'b -> 'c -> 'd -> 'd
    val no_2_num : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'e
    val no_3_num : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_4_num : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_5_num : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_6_num :
      int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_1_num_opt : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'e
    val no_2_num_opt : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'f
    val no_3_num_opt : int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'g
    val no_4_num_opt :
      int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'h
    val no_5_num_opt :
      int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'i
    val no_6_num_opt :
      int -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h -> 'i -> 'j -> 'j
    val no_1_all : int -> 'a -> 'b -> 'c * 'd -> 'e -> 'f -> 'g -> 'g
  end
val ho_aux :
  bool ->
  (int * string Lazy.t) list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'z) option ->
  string -> string list -> ('z -> string) -> ('a -> 'z) -> 'a -> 'z
val choose : bool list -> ('a * 'b) list -> ('a * 'b) list
val ho_aux_no : ('a -> 'z) -> 'a -> 'z
val ho_1_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'z) option ->
  string -> ('a -> string) -> ('z -> string) -> ('a -> 'z) -> 'a -> 'z
val ho_2_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'b -> 'z) option ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('z -> string) -> ('a -> 'b -> 'z) -> 'a -> 'b -> 'z
val ho_3_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'b -> 'c -> 'z) option ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('z -> string) -> ('a -> 'b -> 'c -> 'z) -> 'a -> 'b -> 'c -> 'z
val ho_4_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'z) option ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('z -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'z) -> 'a -> 'b -> 'c -> 'd -> 'z
val ho_5_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'z) option ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('z -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'z) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'z
val ho_6_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z) option ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('z -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'z
val ho_7_opt_aux :
  bool ->
  bool list ->
  bool ->
  ('z -> bool) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h -> 'z) option ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('h -> string) ->
  ('z -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h -> 'z) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'h -> 'z
val ho_1_preopt :
  ('a -> bool) ->
  bool ->
  string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val to_1_preopt :
  ('a -> bool) ->
  bool ->
  string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val ho_1_pre :
  bool ->
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val to_1_pre :
  bool ->
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val to_1 :
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val ho_1_loop :
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
type debug_option =
  DebugCore.debug_option =
    DO_None
  | DO_Trace
  | DO_Loop
  | DO_Both
  | DO_Normal
val string_of_debug_option : debug_option -> string
val debug_map : (string, debug_option) Hashtbl.t
val regexp_line : (Str.regexp * debug_option) list ref
val regexp_line_str : string list ref
val z_debug_file : string ref
val z_debug_flag : bool ref
val debug_file : unit -> in_channel option
val read_from_debug_file : in_channel -> string list
val proc_option : String.t list -> bool * bool
val get_words : string -> string list
val add_entry_with_options :
  (string -> debug_option -> unit) -> string list -> unit
val read_main : unit -> unit
val in_debug : string -> debug_option
val go_1 :
  bool ->
  bool ->
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val go_2 :
  bool ->
  bool ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val go_3 :
  bool ->
  bool ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val go_4 :
  bool ->
  bool ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val go_5 :
  bool ->
  bool ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val go_6 :
  bool ->
  bool ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val go_7 :
  bool ->
  bool ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('h -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
val ho_1 :
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val ho_2 :
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val ho_3 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val ho_4 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val ho_5 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val ho_6 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val ho_7 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('h -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
val splitter : string -> 'a -> ('b -> 'a) -> (bool -> bool -> 'b) -> 'a
val no_1 :
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val no_2 :
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val no_3 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val no_4 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val no_5 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val no_6 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val no_7 :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('h -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'h
val ho_1_opt :
  bool ->
  bool ->
  ('a -> bool) ->
  string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val ho_2_opt :
  bool ->
  bool ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val ho_3_opt :
  bool ->
  bool ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val ho_4_opt :
  bool ->
  bool ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val ho_5_opt :
  bool ->
  bool ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val ho_6_opt :
  bool ->
  bool ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
val no_1_opt :
  ('a -> bool) ->
  string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val no_2_opt :
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val no_3_opt :
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val no_4_opt :
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val no_5_opt :
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val no_6_opt :
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
val add_num : (string -> 'a) -> int -> string -> 'a
val go_1_num :
  bool ->
  bool ->
  int -> string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val go_2_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val go_3_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val go_4_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val go_5_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val go_6_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val add_num_opt : ('a -> string -> 'b) -> int -> 'a -> string -> 'b
val go_1_num_opt :
  bool ->
  bool ->
  int ->
  ('a -> bool) ->
  string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val go_2_num_opt :
  bool ->
  bool ->
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val go_3_num_opt :
  bool ->
  bool ->
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val go_4_num_opt :
  bool ->
  bool ->
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val go_5_num_opt :
  bool ->
  bool ->
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val go_6_num_opt :
  bool ->
  bool ->
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
val to_1_loop :
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val to_2_loop :
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val to_3_loop :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val to_4_loop :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val to_5_loop :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val to_6_loop :
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val to_1_loop_num :
  int -> string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val to_2_loop_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val to_3_loop_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val to_4_loop_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val to_5_loop_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val to_6_loop_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val no_1_num_opt :
  int ->
  ('a -> bool) ->
  string -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val no_2_num_opt :
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val no_3_num_opt :
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val no_4_num_opt :
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val no_5_num_opt :
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val no_6_num_opt :
  int ->
  ('a -> bool) ->
  string ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
val no_1_num :
  int -> string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val no_2_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val no_3_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val no_4_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val no_5_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val no_6_num :
  int ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val ho_1_cmp :
  bool ->
  bool ->
  ('a -> 'b) ->
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val ho_2_cmp :
  bool ->
  bool ->
  ('a -> 'b -> 'c) ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val ho_3_cmp :
  bool ->
  bool ->
  ('a -> 'b -> 'c -> 'd) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val ho_4_cmp :
  bool ->
  bool ->
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val ho_5_cmp :
  bool ->
  bool ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val ho_6_cmp :
  bool ->
  bool ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val no_1_cmp :
  ('a -> 'b) ->
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val no_2_cmp :
  ('a -> 'b -> 'c) ->
  string ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val no_3_cmp :
  ('a -> 'b -> 'c -> 'd) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val no_4_cmp :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val no_5_cmp :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val no_6_cmp :
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  string ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val ho_eff_1 :
  bool ->
  bool ->
  string ->
  bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val ho_eff_2 :
  bool ->
  bool ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val ho_eff_3 :
  bool ->
  bool ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val ho_eff_4 :
  bool ->
  bool ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val ho_eff_5 :
  bool ->
  bool ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val ho_eff_6 :
  bool ->
  bool ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val no_eff_1 :
  string ->
  bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val no_eff_2 :
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val no_eff_3 :
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val no_eff_4 :
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val no_eff_5 :
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val no_eff_6 :
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val ho_eff_1_num :
  bool ->
  bool ->
  int ->
  string ->
  bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val ho_eff_2_num :
  bool ->
  bool ->
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val ho_eff_3_num :
  bool ->
  bool ->
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val ho_eff_4_num :
  bool ->
  bool ->
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val ho_eff_5_num :
  bool ->
  bool ->
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val ho_eff_6_num :
  bool ->
  bool ->
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val no_eff_1_num :
  int ->
  string ->
  bool list -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val no_eff_2_num :
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
val no_eff_3_num :
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
val no_eff_4_num :
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
val no_eff_5_num :
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
val no_eff_6_num :
  int ->
  string ->
  bool list ->
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
  'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
val ho_1_all :
  bool ->
  bool ->
  string ->
  ('a -> bool) ->
  ('b -> 'a) option ->
  bool list -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val ho_2_all :
  bool ->
  bool ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val ho_3_all :
  bool ->
  bool ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val ho_4_all :
  bool ->
  bool ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'e -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val ho_5_all :
  bool ->
  bool ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val ho_6_all :
  bool ->
  bool ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
val ho_1_all_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> bool) ->
  ('b -> 'a) option ->
  bool list -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val ho_2_all_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val ho_3_all_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val ho_4_all_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'e -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val ho_5_all_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val ho_6_all_num :
  bool ->
  bool ->
  int ->
  string ->
  ('a -> bool) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
val no_1_all :
  int ->
  string ->
  ('a -> bool) option ->
  ('b -> 'a) option ->
  bool list -> ('b -> string) -> ('a -> string) -> ('b -> 'a) -> 'b -> 'a
val no_2_all :
  int ->
  string ->
  ('a -> bool) option ->
  ('b -> 'c -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) -> ('a -> string) -> ('b -> 'c -> 'a) -> 'b -> 'c -> 'a
val no_3_all :
  int ->
  string ->
  ('a -> bool) option ->
  ('b -> 'c -> 'd -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('a -> string) -> ('b -> 'c -> 'd -> 'a) -> 'b -> 'c -> 'd -> 'a
val no_4_all :
  int ->
  string ->
  ('a -> bool) option ->
  ('b -> 'c -> 'd -> 'e -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'a
val no_5_all :
  int ->
  string ->
  ('a -> bool) option ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'a) -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a
val no_6_all :
  int ->
  string ->
  ('a -> bool) option ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) option ->
  bool list ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) ->
  ('a -> string) ->
  ('b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a) ->
  'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'a
