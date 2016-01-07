module type INC_TYPE = sig type t val zero : t val inc : t -> t end
module type EQ_TYPE =
  sig type t val eq : t -> t -> bool val string_of : t -> string end
module Basic :
  sig
    exception Bad_string
    exception Bail
    val restart : ('a -> 'b) -> 'a -> 'b
    val string_of_pair :
      ('a -> string) -> ('b -> string) -> 'a * 'b -> string
    val remove_dups : 'a list -> 'a list
    val pr_id : 'a -> 'a
    val pr_string : string -> string
    val print_endline_if : bool -> string -> unit
    val print_string_if : bool -> string -> unit
    val pr_var_prime : string * Globals.primed -> string
    val print_flush : string -> unit
    val pr_no : 'a -> string
    val pr_none : 'a -> string
    val pr_unit : 'a -> string
    val pr_option : ('a -> string) -> 'a option -> string
    val pr_opt : ('a -> string) -> 'a option -> string
    val pr_opt_int : int option -> string
    val pr_pair : ('a -> string) -> ('b -> string) -> 'a * 'b -> string
    val pr_triple :
      ('a -> string) ->
      ('b -> string) -> ('c -> string) -> 'a * 'b * 'c -> string
    val pr_quad :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) -> ('d -> string) -> 'a * 'b * 'c * 'd -> string
    val pr_penta :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('e -> string) -> 'a * 'b * 'c * 'd * 'e -> string
    val pr_hexa :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) -> 'a * 'b * 'c * 'd * 'e * 'f -> string
    val pr_hepta :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
    val pr_quad_ln :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) -> ('d -> string) -> 'a * 'b * 'c * 'd -> string
    val pr_penta_ln :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) -> ('e -> string) -> 'a * 'b * 'c * 'd * 'e -> string
    val pr_hexa_ln :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) -> 'a * 'b * 'c * 'd * 'e * 'f -> string
    val pr_hepta_ln :
      ('a -> string) ->
      ('b -> string) ->
      ('c -> string) ->
      ('d -> string) ->
      ('e -> string) ->
      ('f -> string) ->
      ('g -> string) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
    val pr_add_num : ('a -> string) -> 'a list -> string list
    val pr_lst : string -> ('a -> string) -> 'a list -> string
    val pr_lst_num : string -> ('a -> string) -> 'a list -> string
    val pr_list_brk_sep :
      string -> string -> string -> ('a -> string) -> 'a list -> string
    val pr_list_brk : string -> string -> ('a -> string) -> 'a list -> string
    val pr_list : ('a -> string) -> 'a list -> string
    val pr_list_semi : ('a -> string) -> 'a list -> string
    val pr_list_no_brk : ('a -> string) -> 'a list -> string
    val pr_list_angle : ('a -> string) -> 'a list -> string
    val pr_list_round : ('a -> string) -> 'a list -> string
    val pr_list_round_sep : string -> ('a -> string) -> 'a list -> string
    val pr_list_ln : ('a -> string) -> 'a list -> string
    val pr_list_num : ('a -> string) -> 'a list -> string
    val pr_list_mln : ('a -> string) -> 'a list -> string
    val explode : string -> char list
    val implode : char list -> string
    val map_opt : ('a -> 'b) -> 'a option -> 'b option
    val map_opt_res :
      ('a -> 'b * 'c list) -> 'a option -> 'b option * 'c list
    val fold_opt : ('a -> 'b list) -> 'a option -> 'b list
    val fold_pair1f : ('a -> 'b) -> 'a * 'a -> 'b * 'b
    val fold_pair2f : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
    val map_opt_def : 'a -> ('b -> 'a) -> 'b option -> 'a
    val map_l_snd : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
    val fold_l_snd : ('a -> 'b list) -> ('c * 'a) list -> 'b list
    val fold_l_snd_f :
      ('a -> 'b -> 'a) -> ('c -> 'b) -> 'a -> ('d * 'c) list -> 'a
    val map_l_snd_res :
      ('a -> 'b * 'c) -> ('d * 'a) list -> ('d * 'b) list * 'c list
    val exists_l_snd : ('a -> bool) -> ('b * 'a) list -> bool
    val all_l_snd : ('a -> bool) -> ('b * 'a) list -> bool
    val add_str : string -> ('a -> string) -> 'a -> string
    val opt_to_list : 'a option -> 'a list
    val opt_list_to_list : 'a list option -> 'a list
    val fnone : 'a -> 'a option
    val is_empty : 'a list -> bool
    val is_None : 'a option -> bool
    val last_ne : 'a list -> 'a -> 'a
    val last : 'a list -> 'a
    val spacify : string -> string list -> string
    val split_at : 'a list -> int -> 'a list * 'a list
    val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
    val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
    val map3 :
      ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
    val map4 :
      ('a -> 'b -> 'c -> 'd -> 'e) ->
      'a list -> 'b list -> 'c list -> 'd list -> 'e list
    val repeat : 'a -> int -> 'a list
    val report_error : Globals.loc -> string -> 'a
    val report_warning : Globals.loc -> string -> unit
  end
module HashUti :
  sig
    val copy_keys :
      'a list -> ('a, 'b) Hashtbl.t -> ('a, 'b) Hashtbl.t -> unit
    val list_of_hash_values : ('a, 'b) Hashtbl.t -> 'b list
  end
module BList :
  sig
    val string_of_f : ('a -> string) -> 'a list -> string
    val firsts_last : 'a list -> 'a list * 'a
    val take : int -> 'a list -> 'a list
    val drop : int -> 'a list -> 'a list
    val find_index : ('a -> bool) -> 'a list -> int * 'a
    val list_last : 'a list -> 'a
    val remove_elem_eq : ('a -> 'b -> bool) -> 'b -> 'a list -> 'a list
    val remove_dups_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
    val check_dups_eq : ('a -> 'a -> bool) -> 'a list -> bool
    val check_no_dups_eq : ('a -> 'a -> bool) -> 'a list -> bool
    val subset_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val disjoint_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val overlap_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val find_dups_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
    val find_one_dup_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
    val mem_eq : ('a -> 'b -> bool) -> 'a -> 'b list -> bool
    val intersect_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
    val difference_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
    val diff_split_eq :
      ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'a list
    val list_subset_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val list_setequal_eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
    val list_equiv_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
    val list_find : ('a -> 'b option) -> 'a list -> 'b option
    val add_index : 'a list -> (int * 'a) list
  end
module BListEQ :
  functor (Elt : EQ_TYPE) ->
    sig
      type elem = Elt.t
      type elist = elem list
      val eq : Elt.t -> Elt.t -> bool
      val string_of_elem : Elt.t -> string
      val string_of_f : ('a -> string) -> 'a list -> string
      val firsts_last : 'a list -> 'a list * 'a
      val take : int -> 'a list -> 'a list
      val drop : int -> 'a list -> 'a list
      val find_index : ('a -> bool) -> 'a list -> int * 'a
      val list_last : 'a list -> 'a
      val remove_elem_eq : ('a -> 'b -> bool) -> 'b -> 'a list -> 'a list
      val remove_dups_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
      val check_dups_eq : ('a -> 'a -> bool) -> 'a list -> bool
      val check_no_dups_eq : ('a -> 'a -> bool) -> 'a list -> bool
      val subset_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val disjoint_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val overlap_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val find_dups_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
      val find_one_dup_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
      val mem_eq : ('a -> 'b -> bool) -> 'a -> 'b list -> bool
      val intersect_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
      val difference_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
      val diff_split_eq :
        ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'a list
      val list_subset_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val list_setequal_eq : ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
      val list_equiv_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
      val list_find : ('a -> 'b option) -> 'a list -> 'b option
      val add_index : 'a list -> (int * 'a) list
      val mem : Elt.t -> Elt.t list -> bool
      val string_of : Elt.t list -> string
      val check_dups : Elt.t list -> bool
      val find_dups : Elt.t list -> Elt.t list
      val find_one_dup : Elt.t list -> Elt.t list
      val overlap : Elt.t list -> Elt.t list -> bool
      val intersect : Elt.t list -> Elt.t list -> Elt.t list
      val difference : Elt.t list -> Elt.t list -> Elt.t list
      val list_equal : Elt.t list -> Elt.t list -> bool
    end
exception Stack_Error
class change_flag :
  object
    val mutable cnt : int
    method inc : unit
    method is_change : bool
    method no_change : bool
    method reset : unit
  end
class ['a] stack :
  object ('b)
    val mutable stk : 'a list
    method clone : 'b
    method exists : ('a -> bool) -> bool
    method get : 'a
    method get_stk : 'a list
    method is_avail : bool
    method is_empty : bool
    method len : int
    method mem : 'a -> bool
    method mem_eq : ('a -> 'a -> bool) -> 'a -> bool
    method pop : unit
    method pop_list : 'a list -> unit
    method pop_no_exc : unit
    method pop_top : 'a
    method push : 'a -> unit
    method push_list : 'a list -> unit
    method reset : unit
    method reverse : unit
    method reverse_of : 'a list
    method set_stk : 'a list -> unit
    method top : 'a
  end
class ['a] stack_pr :
  ('a -> string) ->
  ('a -> 'a -> bool) ->
  object ('b)
    val elem_eq : 'a -> 'a -> bool
    val elem_pr : 'a -> string
    val mutable stk : 'a list
    method clone : 'b
    method exists : ('a -> bool) -> bool
    method get : 'a
    method get_stk : 'a list
    method is_avail : bool
    method is_empty : bool
    method len : int
    method mem : 'a -> bool
    method mem_eq : ('a -> 'a -> bool) -> 'a -> bool
    method overlap : 'a list -> bool
    method pop : unit
    method pop_list : 'a list -> unit
    method pop_no_exc : unit
    method pop_top : 'a
    method push : 'a -> unit
    method push_list : 'a list -> unit
    method reset : unit
    method reverse : unit
    method reverse_of : 'a list
    method set_stk : 'a list -> unit
    method string_of : string
    method string_of_no_ln : string
    method string_of_no_ln_rev : string
    method string_of_reverse : string
    method string_of_reverse_log : string
    method top : 'a
  end
class ['a] stack_filter :
  ('a -> string) ->
  ('a -> 'a -> bool) ->
  ('a -> bool) ->
  object ('b)
    val elem_eq : 'a -> 'a -> bool
    val elem_pr : 'a -> string
    val filter_fn : 'a -> bool
    val mutable stk : 'a list
    method clone : 'b
    method exists : ('a -> bool) -> bool
    method filter : unit
    method get : 'a
    method get_stk : 'a list
    method is_avail : bool
    method is_empty : bool
    method len : int
    method mem : 'a -> bool
    method mem_eq : ('a -> 'a -> bool) -> 'a -> bool
    method overlap : 'a list -> bool
    method pop : unit
    method pop_list : 'a list -> unit
    method pop_no_exc : unit
    method pop_top : 'a
    method push : 'a -> unit
    method push_list : 'a list -> unit
    method reset : unit
    method reverse : unit
    method reverse_of : 'a list
    method set_stk : 'a list -> unit
    method string_of : string
    method string_of_no_ln : string
    method string_of_no_ln_rev : string
    method string_of_reverse : string
    method string_of_reverse_log : string
    method string_of_reverse_log_filter : string
    method top : 'a
  end
class ['a] stack_noexc :
  'a ->
  ('a -> string) ->
  ('a -> 'a -> bool) ->
  object ('b)
    val elem_eq : 'a -> 'a -> bool
    val elem_pr : 'a -> string
    val emp_val : 'a
    val mutable stk : 'a list
    method clone : 'b
    method exists : ('a -> bool) -> bool
    method get : 'a
    method get_stk : 'a list
    method is_avail : bool
    method is_empty : bool
    method last : 'a
    method len : int
    method mem : 'a -> bool
    method mem_eq : ('a -> 'a -> bool) -> 'a -> bool
    method overlap : 'a list -> bool
    method pop : unit
    method pop_list : 'a list -> unit
    method pop_no_exc : unit
    method pop_top : 'a
    method pop_top_no_exc : 'a
    method push : 'a -> unit
    method push_list : 'a list -> unit
    method reset : unit
    method reverse : unit
    method reverse_of : 'a list
    method set_stk : 'a list -> unit
    method string_of : string
    method string_of_no_ln : string
    method string_of_no_ln_rev : string
    method string_of_reverse : string
    method string_of_reverse_log : string
    method top : 'a
    method top_no_exc : 'a
  end
class counter :
  int ->
  object
    val mutable ctr : int
    method add : int -> unit
    method get : int
    method inc : unit
    method inc_and_get : int
    method reset : unit
    method string_of : string
  end
module type EQType =
  sig type a val eq : a -> a -> bool val string_of : a -> string end
module EQInt : EQType
module EQList :
  functor (Elt : EQType) ->
    sig
      type a = Elt.a list
      val eq : Elt.a list -> Elt.a list -> bool
      val string_of : Elt.a list -> string
    end
module EQListInt : EQType
module ErrorUti :
  sig
    val error_list : string stack_noexc
    val warning_no : counter
    val no_errors : unit -> bool
    val err : string -> string -> unit
    val error : string -> unit
    val print_errors : unit -> 'a
    val warn : string -> unit
    val warn_if_none : 'a option -> string -> unit
    val fail : string -> 'a
  end
module EqMap :
  functor (Elt : EQ_TYPE) ->
    sig
      type elem = Elt.t
      type key = elem list
      type emap = (elem * key) list
      type epart = elem list list
      type elist = elem list
      type epair = (elem * elem) list
      val eq : Elt.t -> Elt.t -> bool
      val string_of_elem : Elt.t -> string
      val partition : emap -> epart
      val string_of : emap -> string
      val un_partition : epart -> emap
      val mkEmpty : emap
      val is_empty : emap -> bool
      val find_aux : emap -> elem -> key -> key
      val find : emap -> elem -> key
      val find_remove : emap -> elem -> key * emap
      val is_equiv : emap -> elem -> elem -> bool
      val add_equiv : emap -> elem -> elem -> emap
      val build_eset : (elem * elem) list -> emap
      val mem : Elt.t -> Elt.t list -> bool
      val overlap : Elt.t list -> Elt.t list -> bool
      val split_partition : elist -> epart -> epart * epart
      val merge_partition : epart -> epart -> epart
      val domain : emap -> elist
      val get_elems : emap -> elist
      val get_equiv : emap -> epair
      val order_two : 'a list -> 'a list -> 'a list * 'a list
      val merge_eset : emap -> emap -> emap
      val elim_elems_one : emap -> elem -> emap
      val elim_elems : emap -> elem list -> emap
      val find_equiv_all : elem -> emap -> elist
      val find_equiv : elem -> emap -> elem option
      val find_equiv_elim_sure : elem -> emap -> elem option * emap
      val find_equiv_elim : elem -> emap -> (elem * emap) option
      val subs_eset : elem * elem -> emap -> emap
      val subs_eset_par : (elem * elem) list -> emap -> emap
      val check_no_dups : elist -> bool
      val is_one2one : (elem -> elem) -> elist -> bool
      val rename_eset : (elem -> elem) -> emap -> emap
      val rename_eset_with_key : (elem -> elem) -> emap -> emap
      val norm_subs_eq : epair -> epair
      val rename_eset_allow_clash : (elem -> elem) -> emap -> emap
      val build_subs_4_evars : elem list -> emap -> (elem * elem) list
    end
module INT : sig type t = int val zero : int val inc : int -> int end
module IntCtr :
  functor (Elt : INC_TYPE) ->
    sig
      type vtype = Elt.t
      type reftype = vtype ref
      val zero : Elt.t
      val ctr : Elt.t ref
      val reset : unit -> unit
      val inc : unit -> unit
    end
type elem = int
module StackTrace :
  sig
    val ctr : counter
    val debug_stk : int stack_noexc
    val dd_stk : int stack
    val is_same_dd_get : unit -> int option
    val is_same_dd : unit -> bool
    val pop_call : unit -> unit
    val pop_aft_apply_with_exc : ('a -> 'b) -> 'a -> 'b
    val pop_aft_apply_with_exc_no : ('a -> 'b) -> 'a -> 'b
    val string_of : unit -> string
    val push_no_call : unit -> unit
    val push_call_gen : string -> bool -> string * string
    val push_call : string -> string * string
    val push_call_dd : string -> string * string
  end
module type MEM_TYPE =
  sig
    type t
    type ef = t -> t -> bool
    type tlist = t list
    val eq : ef
    val overlap : t -> t -> bool
    val sat : t -> bool
    val intersect : tlist -> tlist -> tlist
    val overlap_eq : ef -> t -> t -> bool
    val intersect_eq : ef -> tlist -> tlist -> tlist
    val star_union : tlist -> tlist -> tlist
    val string_of : t -> string
  end
module type PTR_TYPE =
  sig
    type t
    type ef = t -> t -> bool
    type tlist = t list
    val eq : ef
    val intersect_eq : ef -> tlist -> tlist -> tlist
    val intersect : tlist -> tlist -> tlist
    val string_of : t -> string
  end
module type EQ_PTR_TYPE =
  functor (Elt : EQ_TYPE) ->
    sig
      type a = Elt.t
      type tlist = Elt.t list
      type ef = Elt.t -> Elt.t -> bool
      val intersect : tlist -> tlist -> tlist
      val intersect_eq : ef -> tlist -> tlist -> tlist
    end
module Baga :
  functor (Elt : MEM_TYPE) ->
    sig
      type ptr = Elt.t
      type baga = ptr list
      val mkEmpty : baga
      val eq : Elt.ef
      val overlap : Elt.t -> Elt.t -> bool
      val intersect : Elt.tlist -> Elt.tlist -> Elt.tlist
      val overlap_eq : Elt.ef -> Elt.t -> Elt.t -> bool
      val intersect_eq : Elt.ef -> Elt.tlist -> Elt.tlist -> Elt.tlist
      val star_union : Elt.tlist -> Elt.tlist -> Elt.tlist
      val singleton_baga : ptr -> baga
      val is_dupl_baga_eq : Elt.ef -> baga -> bool
      val is_dupl_baga : baga -> bool
      val is_sat_baga_eq : Elt.ef -> baga -> bool
      val is_sat_baga : baga -> bool
      val star_baga : baga -> baga -> baga
      val conj_baga_eq : Elt.ef -> baga -> baga -> baga
      val conj_baga : baga -> baga -> baga
      val or_baga : baga -> baga -> baga
      val or_baga_eq : Elt.ef -> baga -> baga -> baga
    end
module DisjSet :
  functor (Elt : PTR_TYPE) ->
    sig
      type ptr = Elt.t
      type dlist = ptr list
      type dpart = dlist list
      val eq : Elt.ef
      val intersect : Elt.tlist -> Elt.tlist -> Elt.tlist
      module BL_EQ :
        sig
          type elem = Elt.t
          type elist = elem list
          val eq : Elt.t -> Elt.t -> bool
          val string_of_elem : Elt.t -> string
          val string_of_f : ('a -> string) -> 'a list -> string
          val firsts_last : 'a list -> 'a list * 'a
          val take : int -> 'a list -> 'a list
          val drop : int -> 'a list -> 'a list
          val find_index : ('a -> bool) -> 'a list -> int * 'a
          val list_last : 'a list -> 'a
          val remove_elem_eq : ('a -> 'b -> bool) -> 'b -> 'a list -> 'a list
          val remove_dups_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
          val check_dups_eq : ('a -> 'a -> bool) -> 'a list -> bool
          val check_no_dups_eq : ('a -> 'a -> bool) -> 'a list -> bool
          val subset_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
          val disjoint_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
          val overlap_eq : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
          val find_dups_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
          val find_one_dup_eq : ('a -> 'a -> bool) -> 'a list -> 'a list
          val mem_eq : ('a -> 'b -> bool) -> 'a -> 'b list -> bool
          val intersect_eq :
            ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
          val difference_eq :
            ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list
          val diff_split_eq :
            ('a -> 'b -> bool) -> 'a list -> 'b list -> 'a list * 'a list
          val list_subset_eq :
            ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
          val list_setequal_eq :
            ('a -> 'a -> bool) -> 'a list -> 'a list -> bool
          val list_equiv_eq :
            ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
          val list_find : ('a -> 'b option) -> 'a list -> 'b option
          val add_index : 'a list -> (int * 'a) list
          val mem : Elt.t -> Elt.t list -> bool
          val string_of : Elt.t list -> string
          val check_dups : Elt.t list -> bool
          val find_dups : Elt.t list -> Elt.t list
          val find_one_dup : Elt.t list -> Elt.t list
          val overlap : Elt.t list -> Elt.t list -> bool
          val intersect : Elt.t list -> Elt.t list -> Elt.t list
          val difference : Elt.t list -> Elt.t list -> Elt.t list
          val list_equal : Elt.t list -> Elt.t list -> bool
        end
      val mkEmpty : dpart
      val is_empty : dpart -> bool
      val one_list_dset : dlist -> dpart
      val singleton_dset : ptr -> dpart
      val is_dupl_dset : dpart -> bool
      val is_mem_dset : Elt.t -> dpart -> bool
      val find_diff : (ptr -> ptr -> bool) -> dpart -> ptr -> dpart
      val overlap_q : 'a list -> 'a list -> bool
      val is_disj : (ptr -> ptr -> bool) -> dpart -> ptr -> ptr -> bool
      val remove_dups_disj_set : dpart -> dpart
      val merge_disj_set : dpart -> dpart -> dpart
      val conj_disj_set : dpart -> dpart -> dpart
      val or_disj_set : dpart -> dpart -> dpart
      val star_disj_set : dpart -> dpart -> dpart
      val is_conflict_list : dlist -> bool
      val is_conflict : dpart -> bool
      val is_sat_dset : dpart -> bool
      val apply_subs : ('a * 'a) list -> 'a -> 'a
      val mk_exist_dset : ptr list -> (ptr * ptr) list -> dpart -> dpart
    end
class mult_counters :
  object
    val ctrs : (String.t, int) Hashtbl.t
    method add : string -> int -> unit
    method get : string -> int
    method inc : string -> unit
    method string_of : string
  end
class task_table :
  object
    val tasks : (String.t, float * int * float list) Hashtbl.t
    method add_task_instance : String.t -> float -> unit
    method print : unit
    method print_task_instance : String.t -> unit
  end
module Profiling :
  sig
    val counters : mult_counters
    val tasks : task_table
    val profiling_stack : (String.t * float * bool) stack_noexc
    val add_to_counter : string -> int -> unit
    val inc_counter : string -> unit
    val string_of_counters : unit -> string
    val get_all_time : unit -> float
    val get_main_time : unit -> float
    val get_time : unit -> float
    val push_time_no_cnt : String.t -> unit
    val push_time_always : String.t -> unit
    val push_time : String.t -> unit
    val pop_time_always : String.t -> unit
    val pop_time : String.t -> unit
    val print_info_task : string -> unit
    val print_info : unit -> unit
    val print_counters_info : unit -> unit
    val prof_aux : string -> ('a -> 'z) -> 'a -> 'z
    val do_1 : string -> ('a -> 'z) -> 'a -> 'z
    val do_2 : string -> ('a1 -> 'a2 -> 'z) -> 'a1 -> 'a2 -> 'z
    val do_3 : string -> ('a1 -> 'a2 -> 'a3 -> 'z) -> 'a1 -> 'a2 -> 'a3 -> 'z
    val do_4 :
      string ->
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'z) -> 'a1 -> 'a2 -> 'a3 -> 'a4 -> 'z
    val do_5 :
      string ->
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'z) ->
      'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'z
    val do_6 :
      string ->
      ('a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'z) ->
      'a1 -> 'a2 -> 'a3 -> 'a4 -> 'a5 -> 'a6 -> 'z
    val do_1_num : string -> string -> ('a -> 'b) -> 'a -> 'b
    val do_2_num : string -> string -> ('a -> 'b -> 'c) -> 'a -> 'b -> 'c
    val do_3_num :
      string -> string -> ('a -> 'b -> 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    val do_4_num :
      string ->
      string -> ('a -> 'b -> 'c -> 'd -> 'e) -> 'a -> 'b -> 'c -> 'd -> 'e
    val do_5_num :
      string ->
      string ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f) -> 'a -> 'b -> 'c -> 'd -> 'e -> 'f
    val do_6_num :
      string ->
      string ->
      ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g) ->
      'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g
    val no_1 : 'a -> 'b -> 'b
    val no_2 : 'a -> 'b -> 'b
    val no_3 : 'a -> 'b -> 'b
    val no_4 : 'a -> 'b -> 'b
    val no_5 : 'a -> 'b -> 'b
    val no_6 : 'a -> 'b -> 'b
    val no_1_num : 'a -> 'b -> 'c -> 'c
    val no_2_num : 'a -> 'b -> 'c -> 'c
    val no_3_num : 'a -> 'b -> 'c -> 'c
    val no_4_num : 'a -> 'b -> 'c -> 'c
    val no_5_num : 'a -> 'b -> 'c -> 'c
    val no_6_num : 'a -> 'b -> 'c -> 'c
    val spec_counter : counter
    val gen_time_msg : 'a -> string
    val pop_time_to_s_no_count : String.t -> unit
    val add_index : 'a list -> (int * 'a) list
  end
module SysUti :
  sig
    val qualify_helper_fn : string -> string
    val lib_name : string -> string
    val tmp_name : string -> string
    val extension : string -> string
    val get_path : string -> string
    val trim_quotes : string -> string
    val unescaped : string -> string
    val trim_str : string -> string
    val qualify_if_needed : string -> string -> string
    val unqualify_getting_module : string -> string
    val unqualify : string -> string
    val unprime : string -> string
    val is_primed : string -> bool
    val replace_dot_with_uscore : string -> string
    val replace_minus_with_uscore : string -> string
    val replace_path_sep_with_uscore : string -> string
    val split_by : string -> string -> string list
    val verbose : bool ref
    val amsg : string -> unit
    val msg : string -> unit
    val unsome_safe : 'a option -> 'a -> 'a
    val unsome : 'a option -> 'a
    val is_some : 'a option -> bool
    val string_of_file : string -> string
    val fileLine : string -> string
    val timed_command : string
    val run_with_timeout : string -> int -> int
    val is_breakable : char -> bool
    val new_line_str : string
    val break_lines : string -> string
    val break_lines_num : string -> int -> string
    val break_lines_1024 : string -> string
  end
module Stackable :
  sig
    type 'a tag_elem = 'a * int list
    type 'a tag_list = 'a tag_elem list
    type ('a, 'b) stackable = 'a * 'b list list
    type ('a, 'b) list_of_stackable = ('a, 'b) stackable list
    type 'a ilist = 'a list ref
    val new_ilist : unit -> 'a ilist
    val add_ilist : 'a list -> 'a ilist -> 'a ilist
    val init_level : ('a, 'b) stackable -> ('a, 'b) stackable
    val pushf_add_level :
      ('a -> 'a * 'b list) -> ('a, 'b) stackable -> ('a, 'b) stackable
    val add_level : 'b list -> ('a, 'b) stackable -> ('a, 'b) stackable
    val close_level : ('a, 'b) stackable -> ('a, 'b) stackable
    val collapsef_stack : ('a -> 'b list -> 'a) -> ('a, 'b) stackable -> 'a
    val popf_level :
      ('a -> 'b list -> 'a * 'b list) ->
      ('a, 'b) stackable -> ('a, 'b) stackable
    val init_level_list :
      ('a, 'b) list_of_stackable -> ('a, 'b) list_of_stackable
    val pushf_add_level_list :
      ('a -> 'a * 'b list) ->
      ('a, 'b) list_of_stackable -> ('a, 'b) list_of_stackable
    val collapsef_stack_list :
      ('a -> 'b list -> 'a) -> ('a, 'b) list_of_stackable -> 'a list
    val close_level_list :
      ('a, 'b) list_of_stackable -> ('a, 'b) list_of_stackable
    val popf_level_list :
      ('a -> 'b list -> 'a * 'b list) ->
      ('a, 'b) list_of_stackable -> ('a, 'b) list_of_stackable
    val push_tag : 'a tag_list -> 'a tag_list
    val check_sorted_tag : 'a tag_list -> bool
    val group_tag : 'a tag_list -> 'a tag_list -> ('a tag_list * int) list
    val zip_tag :
      ('a -> 'b -> 'c) -> ('a * int) list -> ('b * int) list -> 'c list
  end
exception Bad_string
exception Bail
val restart : ('a -> 'b) -> 'a -> 'b
val string_of_pair : ('a -> string) -> ('b -> string) -> 'a * 'b -> string
val remove_dups : 'a list -> 'a list
val pr_id : 'a -> 'a
val pr_string : string -> string
val print_endline_if : bool -> string -> unit
val print_string_if : bool -> string -> unit
val pr_var_prime : string * Globals.primed -> string
val print_flush : string -> unit
val pr_no : 'a -> string
val pr_none : 'a -> string
val pr_unit : 'a -> string
val pr_option : ('a -> string) -> 'a option -> string
val pr_opt : ('a -> string) -> 'a option -> string
val pr_opt_int : int option -> string
val pr_pair : ('a -> string) -> ('b -> string) -> 'a * 'b -> string
val pr_triple :
  ('a -> string) ->
  ('b -> string) -> ('c -> string) -> 'a * 'b * 'c -> string
val pr_quad :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) -> ('d -> string) -> 'a * 'b * 'c * 'd -> string
val pr_penta :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('e -> string) -> 'a * 'b * 'c * 'd * 'e -> string
val pr_hexa :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) -> ('f -> string) -> 'a * 'b * 'c * 'd * 'e * 'f -> string
val pr_hepta :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
val pr_quad_ln :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) -> ('d -> string) -> 'a * 'b * 'c * 'd -> string
val pr_penta_ln :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) -> ('e -> string) -> 'a * 'b * 'c * 'd * 'e -> string
val pr_hexa_ln :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) -> ('f -> string) -> 'a * 'b * 'c * 'd * 'e * 'f -> string
val pr_hepta_ln :
  ('a -> string) ->
  ('b -> string) ->
  ('c -> string) ->
  ('d -> string) ->
  ('e -> string) ->
  ('f -> string) ->
  ('g -> string) -> 'a * 'b * 'c * 'd * 'e * 'f * 'g -> string
val pr_add_num : ('a -> string) -> 'a list -> string list
val pr_lst : string -> ('a -> string) -> 'a list -> string
val pr_lst_num : string -> ('a -> string) -> 'a list -> string
val pr_list_brk_sep :
  string -> string -> string -> ('a -> string) -> 'a list -> string
val pr_list_brk : string -> string -> ('a -> string) -> 'a list -> string
val pr_list : ('a -> string) -> 'a list -> string
val pr_list_semi : ('a -> string) -> 'a list -> string
val pr_list_no_brk : ('a -> string) -> 'a list -> string
val pr_list_angle : ('a -> string) -> 'a list -> string
val pr_list_round : ('a -> string) -> 'a list -> string
val pr_list_round_sep : string -> ('a -> string) -> 'a list -> string
val pr_list_ln : ('a -> string) -> 'a list -> string
val pr_list_num : ('a -> string) -> 'a list -> string
val pr_list_mln : ('a -> string) -> 'a list -> string
val explode : string -> char list
val implode : char list -> string
val map_opt : ('a -> 'b) -> 'a option -> 'b option
val map_opt_res : ('a -> 'b * 'c list) -> 'a option -> 'b option * 'c list
val fold_opt : ('a -> 'b list) -> 'a option -> 'b list
val fold_pair1f : ('a -> 'b) -> 'a * 'a -> 'b * 'b
val fold_pair2f : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
val map_opt_def : 'a -> ('b -> 'a) -> 'b option -> 'a
val map_l_snd : ('a -> 'b) -> ('c * 'a) list -> ('c * 'b) list
val fold_l_snd : ('a -> 'b list) -> ('c * 'a) list -> 'b list
val fold_l_snd_f :
  ('a -> 'b -> 'a) -> ('c -> 'b) -> 'a -> ('d * 'c) list -> 'a
val map_l_snd_res :
  ('a -> 'b * 'c) -> ('d * 'a) list -> ('d * 'b) list * 'c list
val exists_l_snd : ('a -> bool) -> ('b * 'a) list -> bool
val all_l_snd : ('a -> bool) -> ('b * 'a) list -> bool
val add_str : string -> ('a -> string) -> 'a -> string
val opt_to_list : 'a option -> 'a list
val opt_list_to_list : 'a list option -> 'a list
val fnone : 'a -> 'a option
val is_empty : 'a list -> bool
val is_None : 'a option -> bool
val last_ne : 'a list -> 'a -> 'a
val last : 'a list -> 'a
val spacify : string -> string list -> string
val split_at : 'a list -> int -> 'a list * 'a list
val split3 : ('a * 'b * 'c) list -> 'a list * 'b list * 'c list
val combine3 : 'a list -> 'b list -> 'c list -> ('a * 'b * 'c) list
val map3 : ('a -> 'b -> 'c -> 'd) -> 'a list -> 'b list -> 'c list -> 'd list
val map4 :
  ('a -> 'b -> 'c -> 'd -> 'e) ->
  'a list -> 'b list -> 'c list -> 'd list -> 'e list
val repeat : 'a -> int -> 'a list
val report_error : Globals.loc -> string -> 'a
val report_warning : Globals.loc -> string -> unit
val qualify_helper_fn : string -> string
val lib_name : string -> string
val tmp_name : string -> string
val extension : string -> string
val get_path : string -> string
val trim_quotes : string -> string
val unescaped : string -> string
val trim_str : string -> string
val qualify_if_needed : string -> string -> string
val unqualify_getting_module : string -> string
val unqualify : string -> string
val unprime : string -> string
val is_primed : string -> bool
val replace_dot_with_uscore : string -> string
val replace_minus_with_uscore : string -> string
val replace_path_sep_with_uscore : string -> string
val split_by : string -> string -> string list
val verbose : bool ref
val amsg : string -> unit
val msg : string -> unit
val unsome_safe : 'a option -> 'a -> 'a
val unsome : 'a option -> 'a
val is_some : 'a option -> bool
val string_of_file : string -> string
val fileLine : string -> string
val timed_command : string
val run_with_timeout : string -> int -> int
val is_breakable : char -> bool
val new_line_str : string
val break_lines : string -> string
val break_lines_num : string -> int -> string
val break_lines_1024 : string -> string
val try_finally : (unit -> 'a) -> ('b -> 'c) -> 'b -> ('a -> 'd) -> 'c
