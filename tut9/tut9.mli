module Keyword : sig val symbols : string list val alphas : string list end
module Lexical :
  sig
    type token = Key of string | Id of string | NumI of Num.num
    exception LexErr of string
    val string_of_token : token -> string
    val is_letter : char -> bool
    val is_digit : char -> bool
    val is_letter_or_digit : char -> bool
    val specials : char list
    val alphanum : char list -> char list -> string * char list
    val numeric : char list -> char list -> Num.num * char list
    val tokenof : string -> token
    val symbolic : char list -> string -> string * char list
    val scanning : char list -> token list -> token list
    val scan : string -> token list
  end
module Parser :
  sig
    exception ParseErr of string
    type ('t, 'r) parse = 't list -> 'r * 't list
    type tokens = Lexical.token list
    val pr_toks : Lexical.token list -> string
    val symbol : string -> tokens -> string * Lexical.token list
    val ident : tokens -> string * Lexical.token list
    val symLetter : char list -> char * char list
    val symChar : char -> char list -> char * char list
    val numb : tokens -> Num.num * Lexical.token list
    val ( >> ) : ('t, 'a) parse -> ('a -> 'b) -> 't list -> 'b * 't list
    val ( |%| ) : ('t, 'a) parse -> ('t, 'a) parse -> 't list -> 'a * 't list
    val ( ++ ) :
      ('t, 'a) parse -> ('t, 'b) parse -> 't list -> ('a * 'b) * 't list
    val empty : 't list -> 'a list * 't list
    val repeat : ('t, 'a) parse -> ('t, 'a list) parse
    val repeat1 : ('t, 'a) parse -> 't list -> 'a list * 't list
    val expect : ('t, 'a) parse -> string -> 't list -> 'a * 't list
    val reader_gen : ('a -> 'b * 'c list) -> ('d -> 'a) -> 'd -> 'b
    val reader : (tokens -> 'a * tokens) -> string -> 'a
  end
val usage_msg : string
val set_source_file : string -> unit
val process_cmd_line : unit -> unit
val key_let : Parser.tokens -> string * Lexical.token list
val key_in : Parser.tokens -> string * Lexical.token list
val key_eq : Parser.tokens -> string * Lexical.token list
val key_plus : Parser.tokens -> string * Lexical.token list
val key_div : Parser.tokens -> string * Lexical.token list
val key_times : Parser.tokens -> string * Lexical.token list
val key_minus : Parser.tokens -> string * Lexical.token list
val key_neg : Parser.tokens -> string * Lexical.token list
val key_open : Parser.tokens -> string * Lexical.token list
val key_close : Parser.tokens -> string * Lexical.token list
type exp =
    ENum of Num.num
  | EId of string
  | EPlus of exp * exp
  | EMinus of exp * exp
  | EDiv of exp * exp
  | ETimes of exp * exp
val string_of_exp : exp -> string
val zero : exp
val expr0 : (Lexical.token, exp) Parser.parse
val expr1 : (Lexical.token, exp) Parser.parse
val factor : (Lexical.token, exp) Parser.parse
val term : (Lexical.token, exp) Parser.parse
val expr2 : (Lexical.token, exp) Parser.parse
val s6 : string
val s7 : string
val s8 : string
val s9 : string
val s10 : string
val s11 : string
val s12 : string
val e1 : string
val e2 : string
val e3 : string
val e4 : string
val test_lex : string -> unit
val test_parse : string -> unit
module Tut9 :
  sig
    val scan_1_i : char list -> string * char list
    val match_str : string -> string -> string * string
    val scan_str : string -> string -> unit
    val scan_tail : (string -> string * string) -> string -> unit
    val q2_i_str : string -> 'a
    val q2_i : string -> unit
    val nt_S : 'a -> 'b
    val test_parse2 : string -> unit
  end
val s1 : string
val s2 : string
val s3 : string
val s4 : string
val s5 : string
val p1 : string
val p2 : string
val p3 : string
val p4 : string
val p5 : string
