val reverse : 'a list -> 'a list
val product : 'a list -> 'b list -> ('a * 'b) list
val product_wo_concat : 'a list -> 'b list -> ('a * 'b) list list
val product_column_major : 'a list -> 'b list -> ('a * 'b) list list
val product2 : 'a list -> 'a list -> ('a * 'a) list
val divisor_prod : int list -> int list -> int list
val count_nums1 : int list -> int * int * int
val count_nums2 : int list -> int * int * int
type 'a tree = Leaf of 'a | Node of 'a * 'a tree * 'a tree
val t1 : int tree
val t2 : int tree
val t3 : int tree
val map_tree : ('a -> 'b) -> 'a tree -> 'b tree
val fold_tree : ('a -> 'b) -> ('a -> 'b -> 'b -> 'b) -> 'a tree -> 'b
val add_n : int tree -> int -> int tree
val right_most : 'a tree -> 'a
val mirror_tree : 'a tree -> 'a tree
val t4 : char tree
val add_size_temp : 'a tree -> 'a list * ('a list * 'a) tree
val add_size : 'a tree -> (int * 'a) tree
val t5 : int tree
val check_bst : 'a tree -> bool
val fold_tree_preorder : ('z -> 'a -> 'z) -> 'z -> 'a tree -> 'z
val count_tree : 'a tree -> int
val pr_tree : ('a -> string) -> 'a tree -> string
val pr_tree2 : ('a -> string) -> 'a tree -> string
val pr_tree_infix : ('a -> string) -> 'a tree -> string
val test : int tree -> unit
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val pr_list2 : ('a -> string) -> 'a list -> string
val add_num : 'a list -> (int * 'a) list
val ls : string list
val pr_id : 'a -> 'a
val pr_list_num : string -> (string -> string) -> string list -> string
val test_num : string -> (string -> string) -> string list -> unit
val wrapper :
  ('a -> 'v) ->
  ('v -> 'b -> unit) -> ('v -> exn -> unit) -> ('a -> 'b) -> 'a -> 'b
val out_print : string -> unit
val tracer :
  string -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val fib : int -> int
val fib1 : int -> int
val fib2 : int -> int
val trace_test :
  string ->
  ('a -> bool) -> ('a -> string) -> ('b -> string) -> ('a -> 'b) -> 'a -> 'b
val fib3 : int -> int
val aux : int -> int
