val ramification_entailments : int ref
val noninter_entailments : int ref
val total_entailments : int ref
type aliasing_scenario =
    Not_Aliased
  | May_Aliased
  | Must_Aliased
  | Partial_Aliased
type ('a, 'b) twoAns = FstAns of 'a | SndAns of 'b
type ident = string
type constant_flow = string
exception Illegal_Prover_Format of string
exception SA_HP_TUPLED
val reverify_flag : bool ref
val reverify_all_flag : bool ref
val ineq_opt_flag : bool ref
val illegal_format : string -> 'a
type lemma_kind =
    LEM_TEST
  | LEM_TEST_NEW
  | LEM
  | LEM_UNSAFE
  | LEM_SAFE
  | LEM_INFER
type flags = Flag_str of string | Flag_int of int | Flag_float of float
type bformula_label = int
and ho_branch_label = string
type formula_label = int * string
and control_path_id_strict = formula_label
and control_path_id = control_path_id_strict option
val eq_control_path_id : formula_label -> formula_label -> bool
val empty_label : int * string
val app_e_l : 'a -> (int * string) * 'a
val combine_lbl : 'a * string -> 'a * string -> 'a * string
type path_label = int
type path_trace = (control_path_id_strict * path_label) list
and loc = {
  start_pos : Lexing.position;
  mid_pos : Lexing.position;
  end_pos : Lexing.position;
}
and primed = Primed | Unprimed
and heap_ann = Lend | Imm | Mutable | Accs
and vp_ann = VP_Zero | VP_Full | VP_Value
and term_ann = Term | Loop | MayLoop | Fail of term_fail
and term_fail = TermErr_May | TermErr_Must
and rel = REq | RNeq | RGt | RGte | RLt | RLte | RSubAnn
type hp_arg_kind = I | NI
val print_arg_kind : hp_arg_kind -> string
type typ =
    UNK
  | TVar of int
  | AnnT
  | Bool
  | Float
  | Int
  | INFInt
  | NUM
  | Void
  | List of typ
  | BagT of typ
  | Named of ident
  | Array of (typ * int)
  | RelT of typ list
  | HpT
  | Tree_sh
  | Pointer of typ
val is_program_pointer : ident -> bool * ident
val is_pointer_typ : typ -> bool
val convert_typ : typ -> typ
val revert_typ : typ -> typ
val name_of_typ : typ -> string
val is_pointer : typ -> bool
val barrierT : typ
val convert_prim_to_obj : typ -> typ
val hp_default_prefix_name : string
val hppost_default_prefix_name : string
val dang_hp_default_prefix_name : string
type mode = ModeIn | ModeOut
type perm_type = NoPerm | Frac | Count | Dperm
val perm : perm_type ref
val no_pos : loc
val is_no_pos : loc -> bool
val is_float_type : typ -> bool
val string_of_heap_ann : heap_ann -> string
val int_of_heap_ann : heap_ann -> int
val string_of_vp_ann : vp_ann -> string
val string_of_term_ann : term_ann -> string
val string_of_pos_plain : Lexing.position -> string
val line_number_of_pos : loc -> string
val proof_logging : bool ref
val proof_logging_txt : bool ref
val log_proof_details : bool ref
val proof_logging_time : float ref
val sleek_logging_txt : bool ref
val dump_proof : bool ref
val dump_sleek_proof : bool ref
class ['a] store :
  'a ->
  ('a -> string) ->
  object
    val emp_val : 'a
    val mutable lc : 'a option
    method dump : unit
    method get : 'a
    method get_rm : 'a
    method is_avail : bool
    method reset : unit
    method set : 'a -> unit
    method string_of : string
  end
class failure_mode :
  object
    val emp_val : bool
    val mutable lc : bool option
    method dump : unit
    method get : bool
    method get_rm : bool
    method is_avail : bool
    method reset : unit
    method set : bool -> unit
    method string_of : string
  end
class prog_loc :
  object
    val emp_val : loc
    val mutable lc : loc option
    method dump : unit
    method get : loc
    method get_rm : loc
    method is_avail : bool
    method reset : unit
    method set : loc -> unit
    method string_of : string
    method string_of_pos : string
  end
val proving_loc : prog_loc
val post_pos : prog_loc
val explain_mode : failure_mode
val return_exp_pid : control_path_id list ref
val z3_proof_log_list : string list ref
val z3_time : float ref
val add_to_z3_proof_log_list : string -> unit
val entail_pos : loc ref
val set_entail_pos : loc -> unit
val pr_lst : string -> ('a -> string) -> 'a list -> string
val pr_list_brk : string -> string -> ('a -> string) -> 'a list -> string
val pr_list : ('a -> string) -> 'a list -> string
val pr_list_angle : ('a -> string) -> 'a list -> string
val pr_list_round : ('a -> string) -> 'a list -> string
val string_of_typ : typ -> string
val is_RelT : typ -> bool
val is_HpT : typ -> bool
val string_of_typ_alpha : typ -> ident
val subs_tvar_in_typ : typ -> int -> typ -> typ
val null_type : typ
val is_null_type : typ -> bool
val s_i_list : string list -> string -> string
val string_of_ident_list : string list -> string
val string_of_primed : primed -> string
val string_of_primed_ident : string * primed -> string
val pr_ident_list : (string * primed) list -> string
val s_p_i_list : (string * primed) list -> string -> string
val string_of_primed_ident_list : (string * primed) list -> string
val is_substr : string -> string -> bool
val is_dont_care_var : string -> bool
val idf : 'a -> 'a
val idf2 : 'a -> 'b -> 'a
val nonef : 'a -> 'b option
val voidf : 'a -> unit
val voidf2 : 'a -> 'b -> unit
val somef : 'a -> 'a option
val or_list : bool list -> bool
val and_list : bool list -> bool
val push_opt_void_pair : 'a option -> ('a * unit) option
val push_opt_val : 'a option -> 'b -> ('a * 'b) option
val push_opt_val_rev : 'a option -> 'b -> ('b * 'a) option
val no_pos1 : Lexing.position
val res_name : string
val null_name : string
val sl_error : string
val logical_error : string
val fnc_error : string
val lemma_error : string
val undefined_error : string
val timeout_error : string
val eres_name : string
val self : string
val constinfinity : string
val deep_split_disjuncts : bool ref
val check_integer_overflow : bool ref
val this : string
val is_self_ident : string -> bool
val thread_name : string
val thread_typ : typ
val proc_typ : typ
val fork_name : string
val join_name : string
val init_name : string
val finalize_name : string
val acquire_name : string
val release_name : string
val lock_name : string
val lock_typ : typ
val ls_name : string
val lsmu_name : string
val ls_data_typ : string
val waitlevel_name : string
val waitlevel_typ : typ
val level_pred : string
val level_name : string
val level_data_typ : typ
val ls_typ : typ
val lsmu_typ : typ
val silence_output : bool ref
val header_file_list : string list ref
val pragma_list : string list ref
val lib_files : string list ref
val ptr_to_int_exact : bool ref
val is_sleek_running : bool ref
val remove_label_flag : bool ref
val label_split_conseq : bool ref
val label_split_ante : bool ref
val label_aggressive_sat : bool ref
val label_aggressive_imply : bool ref
val texify : bool ref
val testing_flag : bool ref
val instantiation_variants : int ref
val omega_simpl : bool ref
val no_simpl : bool ref
val source_files : string list ref
val input_file_name : string ref
val use_split_match : bool ref
val consume_all : bool ref
val enable_split_lemma_gen : bool ref
val dis_show_diff : bool ref
val sa_print_inter : bool ref
val tc_drop_unused : bool ref
val simpl_unfold3 : bool ref
val simpl_unfold2 : bool ref
val simpl_unfold1 : bool ref
val simpl_memset : bool ref
val print_heap_pred_decl : bool ref
val cond_path_trace : bool ref
val pred_syn_modular : bool ref
val syntatic_mode : bool ref
val sa_dnc : bool ref
val pred_reuse : bool ref
val pred_en_oblg : bool ref
val pred_syn_flag : bool ref
val sa_syn : bool ref
val lemma_syn : bool ref
val sa_en_split : bool ref
val pred_split : bool ref
val sa_refine_dang : bool ref
val pred_elim_useless : bool ref
val infer_deep_ante_flag : bool ref
val pred_infer_flag : bool ref
val pred_elim_dangling : bool ref
val sa_sp_split_base : bool ref
val sa_pure_field : bool ref
val sa_infer_split_base : bool ref
val pred_elim_unused_preds : bool ref
val sa_unify_dangling : bool ref
val pred_conj_unify : bool ref
val pred_disj_unify : bool ref
val pred_equiv : bool ref
val sa_tree_simp : bool ref
val sa_subsume : bool ref
val norm_extract : bool ref
val allow_norm_disj : bool ref
val norm_cont_analysis : bool ref
val lemma_infer : bool ref
val dis_sem : bool ref
val procs_verified : string list ref
val false_ctx_line_list : loc list ref
val b_datan : string
val verify_callees : bool ref
val elim_unsat : bool ref
val disj_compute_flag : bool ref
val compute_xpure_0 : bool ref
val inv_wrap_flag : bool ref
val lhs_case_flag : bool ref
val lhs_case_search_flag : bool ref
val smart_xpure : bool ref
val super_smart_xpure : bool ref
val precise_perm_xpure : bool ref
val smart_memo : bool ref
val elim_exists_ff : bool ref
val allow_imm : bool ref
val allow_imm_inv : bool ref
val allow_field_ann : bool ref
val allow_mem : bool ref
val infer_mem : bool ref
val pa : bool ref
val allow_inf : bool ref
val ann_derv : bool ref
val print_ann : bool ref
val print_derv : bool ref
val print_clean_flag : bool ref
val is_deployed : bool ref
val print_assume_struc : bool ref
val web_compile_flag : bool ref
val allow_norm : bool ref
val allow_ls : bool ref
val allow_locklevel : bool ref
val ann_vp : bool ref
val allow_ptr : bool ref
val print_proc : bool ref
val check_all : bool ref
val auto_number : bool ref
val sleek_flag : bool ref
val sleek_log_filter : bool ref
val use_field : bool ref
val large_bind : bool ref
val print_x_inv : bool ref
val print_cnv_null : bool ref
val hull_pre_inv : bool ref
val use_coercion : bool ref
val case_split : bool ref
val simplified_case_normalize : bool ref
val use_set : bool ref
val consistency_checking : bool ref
val wrap_exist : bool ref
val move_exist_to_LHS : bool ref
val max_renaming : bool ref
val anon_exist : bool ref
val simplify_pure : bool ref
val enable_norm_simp : bool ref
val print_version_flag : bool ref
val elim_exists_flag : bool ref
val filtering_flag : bool ref
val split_rhs_flag : bool ref
val n_xpure : int ref
val verbose_num : int ref
val fixcalc_disj : int ref
val pre_residue_lvl : int ref
val check_coercions : bool ref
val num_self_fold_search : int ref
val self_fold_search_flag : bool ref
val show_gist : bool ref
val imply_top_flag : bool ref
val early_contra_flag : bool ref
val trace_failure : bool ref
val trace_all : bool ref
val print_mvars : bool ref
val print_type : bool ref
val wrap_exists_implicit_explicit : bool ref
val profiling : bool ref
val enable_syn_base_case : bool ref
val enable_case_inference : bool ref
val print_core : bool ref
val print_core_all : bool ref
val print_err_sleek : bool ref
val enable_prune_cache : bool ref
val enable_counters : bool ref
val enable_time_stats : bool ref
val enable_count_stats : bool ref
val enable_fast_imply : bool ref
val failure_analysis : bool ref
val seq_to_try : bool ref
val print_input : bool ref
val print_input_all : bool ref
val print_cil_input : bool ref
val disable_failure_explaining : bool ref
val simplify_error : bool ref
val prune_cnt_limit : int ref
val suppress_warning_msg : bool ref
val disable_elim_redundant_ctr : bool ref
val enable_strong_invariant : bool ref
val enable_aggressive_prune : bool ref
val enable_redundant_elim : bool ref
val enable_constraint_based_filtering : bool ref
val enulalias : bool ref
val pass_global_by_value : bool ref
val exhaust_match : bool ref
val memo_verbosity : int ref
val profile_threshold : float
val no_cache_formula : bool ref
val simplify_imply : bool ref
val enable_incremental_proving : bool ref
val disable_multiple_specs : bool ref
val perm_prof : bool ref
val cp_test : bool ref
val cp_prefile : bool ref
val gen_cpfile : bool ref
val file_cp : string ref
val cpfile : string ref
val f_1_slice : bool ref
val f_2_slice : bool ref
val no_memoisation : bool ref
val no_incremental : bool ref
val no_LHS_prop_drop : bool ref
val no_RHS_prop_drop : bool ref
val do_sat_slice : bool ref
val dis_term_chk : bool ref
val term_verbosity : int ref
val dis_call_num : bool ref
val dis_phase_num : bool ref
val term_reverify : bool ref
val dis_bnd_chk : bool ref
val dis_term_msg : bool ref
val dis_post_chk : bool ref
val dis_ass_chk : bool ref
val log_filter : bool ref
val en_slc_ps : bool ref
val override_slc_ps : bool ref
val dis_ps : bool ref
val dis_slc_ann : bool ref
val slicing_rel_level : int ref
val dis_slicing : bool ref
val opt_imply : int ref
val opt_ineq : bool ref
val infer_slicing : bool ref
val infer_lvar_slicing : bool ref
val multi_provers : bool ref
val is_sat_slicing : bool ref
val delay_case_sat : bool ref
val force_post_sat : bool ref
val delay_if_sat : bool ref
val delay_proving_sat : bool ref
val disable_assume_cmd_sat : bool ref
val disable_pre_sat : bool ref
val do_infer_inv : bool ref
val opt_classic : bool ref
val do_classic_frame_rule : bool ref
type ensures_type = bool option
type assert_type = bool option
type entail_type = bool option
val do_abd_from_post : bool ref
val unable_to_fold_rhs_heap : bool ref
val domain_name : string ref
val do_infer_inc : bool ref
val add_count : int ref -> unit
val omega_err : bool ref
val seq_number : int ref
val sat_timeout_limit : float ref
val user_sat_timeout : bool ref
val imply_timeout_limit : float ref
val dis_provers_timeout : bool ref
val sleek_timeout_limit : float ref
val branch_point_id : int ref
val reset_formula_point_id : unit -> unit
val iast_label_table :
  (control_path_id * string * (control_path_id * path_label * loc) list * loc)
  list ref
val locs_of_path_trace : path_trace -> loc list
val locs_of_partial_context : (path_trace * 'a) list * 'b -> loc list
val fresh_formula_label : string -> formula_label
val fresh_branch_point_id : string -> control_path_id
val fresh_strict_branch_point_id : string -> control_path_id_strict
val eq_formula_label : formula_label -> formula_label -> bool
val seq_number2 : int ref
val fresh_int2 : unit -> int
val reset_int2 : unit -> unit
val fresh_int : unit -> int
val fresh_ty_var_name : typ -> int -> string
val fresh_var_name : string -> int -> string
val fresh_trailer : unit -> string
val fresh_any_name : string -> string
val fresh_name : unit -> string
val fresh_label : loc -> string
val fresh_names : int -> string list
val formula_cache_no_series : int ref
val fresh_formula_cache_no : unit -> int
val gen_ext_name : string -> string -> string
val string_of_loc : loc -> string
val string_of_pos : Lexing.position -> string
val string_of_full_loc : loc -> string
val string_of_loc_by_char_num : loc -> string
val string_of_formula_label : formula_label -> string
val seq_local_number : int ref
val fresh_local_int : unit -> int
val fresh_local_var_name : string -> string
val join2 : 'a -> 'b -> 'a * 'b
val fst3 : 'a * 'b * 'c -> 'a
val snd3 : 'a * 'b * 'c -> 'b
val change_fst3 : 'a * 'b * 'c -> 'd -> 'd * 'b * 'c
val path_trace_eq : (('a * 'b) * 'c) list -> (('a * 'd) * 'c) list -> bool
val path_trace_lt : (('a * 'b) * 'c) list -> (('a * 'd) * 'c) list -> bool
val path_trace_gt : (('a * 'b) * 'c) list -> (('a * 'd) * 'c) list -> bool
val dummy_exception : unit -> unit
val bin_op_to_list :
  string -> ('a -> (string * 'a list) option) -> 'a -> 'a list
val bin_to_list : ('a -> (string * 'a list) option) -> 'a -> string * 'a list
val print_proof : bool ref
val strquote : string -> string
val norm_file_name : string -> string
val proof_no : int ref
val next_proof_no : unit -> int
val get_proof_no : unit -> int
val get_proof_no_str : unit -> string
val sleek_proof_no : int ref
val last_sleek_fail_no : int ref
val get_sleek_no : unit -> int
val set_sleek_no : int -> unit
val get_last_sleek_fail : unit -> int
val set_last_sleek_fail : unit -> unit
