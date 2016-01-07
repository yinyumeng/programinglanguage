val last : 'a list -> 'a option
val last_two : 'a list -> 'a * 'a
val insert : 'a -> 'a list -> 'a list
val sort : 'a list -> 'a list
type uprim = I of int | F of float | S of string
val mix_ls : uprim list
val value_of_mix : uprim -> int
val sum_of_mix_list : uprim list -> int
type ('a, 'b) sum = L of 'a | R of 'b
type uprim2 = (int, (float, string) sum) sum
val mk_I : int -> (int, 'a) sum
val mk_F : float -> ('a, (float, 'b) sum) sum
val mk_S : string -> ('a, ('b, string) sum) sum
val mix_ls2 : (int, (float, string) sum) sum list
val value_of_mix2 : (int, (float, string) sum) sum -> int
val sum_of_mix_list2 : uprim2 list -> int
val reult : int
type 'aa btree = Leaf of 'aa | Node of 'aa * 'aa btree * 'aa btree
val t1 : int btree
val t2 : int btree
val max_tree : int btree -> int
val flatten_infix : 'a btree -> 'a list
val flatten_prefix : 'a btree -> 'a list
val power : int -> int -> int
val power_tail : int -> int -> int
val power_logn : int -> int -> int
