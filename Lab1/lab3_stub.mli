val align_w_penalty : 'a -> 'b -> 'c -> 'd
class ['a, 'b] memo_table :
  object
    val tab : ('a, 'b) Hashtbl.t
    method get : 'a -> 'b option
    method store : 'a -> 'b -> unit
  end
class ['a, 'b] memo_table_ho :
  object
    val tab : ('a, 'b) Hashtbl.t
    method compute : ('a -> 'b) -> 'a -> 'b
    method get : 'a -> 'b option
    method store : 'a -> 'b -> unit
  end
val align : string -> string -> int
val align_print : 'a -> 'b -> 'c -> 'd
