(****************************************)
(* Lab 2/Tut 4 : Higher-Order Functions *)
(****************************************)

(* We will be attempting these questions during tut/lab session on 14Sept *)
(* This lab assignment is to be submitted by 23rd Sept 2015 6pm *)
(* In case you have problems, please see your lecturer or tutor for consultation *)

(* 
  Q1: Consider the following implementation of reverse
      using List.fold_left. It contains a bug which causes its
      result to be wrong. Can you fix it so that you get the following
      correct outcomes?

# reverse2 [1;2;3];;
- : int list = [3; 2; 1]
# reverse2 [1;2];;
- : int list = [2; 1]
# reverse2 [];;
- : 'a list = []

*)

(* fix a bug in the code below *)
let reverse (xs:'a list) : 'a list =
  List.fold_left (fun acc x -> x::acc) ([]) xs;;


(* 
  Q2 : List Operations

  Using only List.map, List.filter and List.concat,
	concat: a list * a list -> a list
  write code for the following list operations. You
  may use let constructs to name intermediate computations.

*)

(* 
   (a) cross-product of two lists that returns an integer
       from each list as a pair (a,b). 
       That is [(a,b) | a <- xs, b <- ys]
   Example: product [1;2;3] ['a';'b'];;  
   ==> 
    (int * char) list =
    [(1, 'a'); (1, 'b'); (2, 'a'); (2, 'b'); (3, 'a'); (3, 'b')]
*)

let product xs ys = 
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	
let product_wo_concat xs ys = 
	List.map (fun x -> List.map (fun y -> (x,y)) ys) xs

let product_column_major xs ys = 
	List.map (fun y -> List.map (fun x -> (x,y)) xs) ys

(* 
   (b) cross-product of two lists that returns an integer
       from each list as a pair (a,b) such that a<=b.
       That is [(a,b) | a <- xs, b <- ys, a<=b]
   Example : product2 [1;2;3] [2;7];;  
   ==> 
- : (int * int) list = [(1, 2); (1, 7); (2, 2); (2, 7); (3, 7)]
*)

let product2 xs ys = 
	let res = List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	in List.filter(fun(x,y) -> x<=y) res

(*
let product2_second xs ys = 
	List.concat (List.map (fun x ->  List.map ( fun y -> (x,y)  ) ys) xs)
	
	(List.filter(fun(x,y) -> x<=y)*)

(* 
   (c) The divisor from a product of two lists, but only 
       if the second element is non-zero.
       That is [a/b | b<-ys, b!=0, a <- xs]
   Example : divisor_prod [5;9;4] [2;0;3];;  
   ==> 
   - : int list = [2; 4; 2; 1; 3; 1]
*)


let divisor_prod xs ys = 
	List.concat (List.map (fun y -> List.map (fun x -> (x/y))  xs)(List.filter(fun y -> y!=0) ys))
   
(* 
  Q3 : Write a function that would count the number of
       positive, negative and zero elements in a list of numbers.

      (i) Use only the List.fold_right operation
      (ii) Use only List.filter and List.length

   Example:
     count_nums1 [1;-4;5;8;0;-9];;
     - : int * int * int = (3, 2, 1)

*)

(* to return number of positive, negative and zero numbers *)
let count_nums1 (ys:int list) : (int * int * int) =
	List.fold_left (fun (acc1,acc2,acc3) x -> 
		if x>0 then (acc1+1,acc2,acc3)
		else if x<0 then (acc1,acc2+1,acc3)
		else (acc1,acc2,acc3+1)) (0,0,0) ys;;

let count_nums2 (ys:int list) : (int * int * int) =
	let acc1 = List.length (List.filter(fun y -> y>0) ys) in
	let acc2 = List.length (List.filter(fun y -> y<0) ys) in
	let acc3 = List.length (List.filter(fun y -> y==0) ys) in
	(acc1,acc2,acc3)
	
	


(* 
  Q4 : Higher-Order functions for Trees

	During the last tutorial, we implemented two higher-order
        functions for a simple binary trees as follows:

  Let us practise more examples on them.

*)

type 'a tree = 
  | Leaf of 'a
  | Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,Leaf 6);;
let t3 = Node (5,t2,Leaf 3);;

(* map for tree *)
let rec map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
  match t with
    | Leaf x -> Leaf (f x)
    | Node (x,lt,rt) -> Node (f x,(map_tree f lt),(map_tree f rt))

let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
  let rec aux t =
    match t with
      | Leaf x -> f1 x
      | Node (x,lt,rt) -> f2 x (aux lt) (aux rt) 
  in aux t

(* Use these higher-order functions to write the following codes *)

(* (a) a function that would add n to every element of a tree 

Example:
# t1;;
- : int tree = Node (3, Leaf 1, Leaf 2)
# add_n t1 3;;
- : int tree = Node (6, Leaf 4, Leaf 5)
*)

let add_n (t:int tree) (n:int) : int tree = 
	let rec aux t1 n =
		fold_tree
  	(fun a ->  Leaf (a+n))
  	(fun a tl tr ->
			 Node(a+n, tl ,tr )) t1 in aux t n
(* (b) a function that would return the rightmost element of a tree 

# t2;;
- : int tree = Node (4, Node (3, Leaf 1, Leaf 2), Leaf 6)
# right_most t2;;
- : int = 6
# t3;;
- : int tree = Node (5, Node (4, Node (3, Leaf 1, Leaf 2), Leaf 6), Leaf 3)
# right_most t3;;
- : int = 3
*)

let right_most (t:'a tree) : 'a = 
  fold_tree
  	(fun a -> a)
  	(fun a r1 rr -> rr)
  	t
	
(* (c) a function that would return the mirror of a tree 
        where left and right subtrees are recursively flipped 

# t2;;
- : int tree = Node (4, Node (3, Leaf 1, Leaf 2), Leaf 6)
# mirror_tree t2;;
- : int tree = Node (4, Leaf 6, Node (3, Leaf 2, Leaf 1))
: 'a tree
*)

let mirror_tree (t:'a tree)  = 
  fold_tree
  	(fun a ->  Leaf (a))
  	(fun a tl tr ->
			 Node(a, tr,tl))
  	t

let t4 = Node ('a',Leaf 'b',Node ('c',Leaf 'e',Leaf 'f'));;

(* (d) a function that would return a tree with its
       sized information for each sub-trees
       tagged to each of its elements 
Node ((4, 5), Node ((3, 3), Leaf (1, 1), Leaf (1, 2)), Leaf (1, 6))
# t4;;
- : char tree = Node ('a', Leaf 'b', Node ('c', Leaf 'e', Leaf 'f'))

# add_size t4;;
- : (int * char) tree =
Node ((5, 'a'), Leaf (1, 'b'), Node ((3, 'c'), Leaf (1, 'e'), Leaf (1, 'f')))

*)

let add_size_temp (t:'a tree) = 
	fold_tree
  	(fun a -> ([a], Leaf ([a],a)))
  	(fun a (ll,tl) (lr,tr) ->
			(ll@[a]@lr, Node((ll@[a]@lr,a), tl,tr)))
  	t

let rec add_size (t:'a tree) : (int * 'a)  tree= 
	snd(	fold_tree
  	(fun a -> (1, Leaf (1,a)))
  	(fun a (ll,tl) (lr,tr) ->((ll+1+lr), Node(((ll+1+lr),a), tl,tr)))
  	t)


(* (e) Write a function to check if a tree of integer is
       actually a binary search tree where all the elements are sorted
       in the following ordering. All elements in the left subtree
       are strictly smaller than the root node, which is in turn
       smaller or equal to all elements in the right sub-tree.

# t1;;
- : int tree = Node (3, Leaf 1, Leaf 2)
# check_bst t1;;
- : bool = false
# t5;;
- : int tree = Node (2, Leaf 1, Leaf 3)
# check_bst t5;;
- : bool = true
*)

let t5 = Node(2,Leaf 1, Leaf 3);;

let check_bst t =
	snd(fold_tree
  	(fun a -> (a,true))
  	(fun a (ll,tl) (lr,tr) ->
			(a,(ll<a && a<=lr && tl && tr)))
  	t)

(* 
  Q5 : The fold_tree operation uses tree recursion.

      Let us write a different fold tree operation that works
      with the help of accumulating parameter that would 
      be similar to List.fold_left.
 
      An example of this which uses pre-order traversal is 
      given below.
*)

let fold_tree_preorder (f:'z->'a->'z) (z:'z) (t:'a tree) : 'z = 
  let rec aux z t = match t with
    | Leaf a -> f z a
    | Node (a,lt,rt) -> 
      let z1 = f z a in
      let z2 = aux z1 lt in
      aux z2 rt
  in aux z t;;

(* Using fold_tree_preorder, write two functions below *)
(* 
(a) count the number of elements in the tree 
Example:
# t4;;
- : char tree = Node ('a', Leaf 'b', Node ('c', Leaf 'e', Leaf 'f'))
# count_tree t4;;
- : int = 5
*)
let count_tree (t:'a tree) : int =  
	fold_tree_preorder
		(fun acc a-> acc+1)
  	0 t
(*failwith "TBI with fold_tree_preorder (...) (..) t"*)

(* (b) Compare this fold_tree_preorder with fold_tree.
   Can one be implemented in terms of the other, or 
   are they incomparable?
*)
(* they are incomparable*)

(*

	Q6: Pretty printers.

	Consider the binary tree defined earlier. 

	You have been given a higher-order printer which prints the tree in a pre-fix form.
	As an example, the tree t2 would be printed as:

Node 4
Node 3
Leaf 1
Leaf 2
Node 3
Leaf 1
Leaf 2

	(i) pretty printer
	This above printing is however less readable and you are asked to provide
	a neater printer that would provide space indentation to represent
	the depth of each subtrees.

	Implement pr_tree2, so that it would provide such space indentation
	for each new level of the subtrees, as illustrated below:

Node 4
 Node 3
  Leaf 1
  Leaf 2
 Node 3
  Leaf 1
  Leaf 2

	(i) infix printer.
	One may prefer a tree printer that is presented in an infix manner.
	Write a new pr_tree_infix method that would allow your binary tree to be printed
	in an infix order. The output for t2 example is illustrated below:

  Leaf 1
 Node 3
  Leaf 2
Node 4
  Leaf 1
 Node 3
  Leaf 2

*)

let pr_tree (pr:'a->string) (xs:'a tree) : string 
 = let rec aux xs = 
		match xs with
		| Leaf e -> "Leaf "^(pr e)^("\n")
		| Node (e,lt,rt) -> 
				"Node "^(pr e)^("\n")
				 ^(aux lt)^(aux rt)
	in aux xs;;

(*print_string (pr_tree (fun x -> string_of_int x) t2)*)

(* please change failwith .. to your implementation *)
let pr_tree2 (pr:'a->string) (xs:'a tree) : string 
= let rec aux xs space= 
		let space_new = " " ^space in 
  		match xs with
  		| Leaf e -> space^"Leaf "^(pr e)^("\n")
  		| Node (e,lt,rt) -> 
  				space^"Node "^(pr e)^("\n")
  				 ^(aux lt space_new)^(aux rt space_new)
	in aux xs "";;

	(*= failwith "neat tree printer with nested indentation"*)

(* please change failwith .. to your implementation *)
let pr_tree_infix (pr:'a->string) (xs:'a tree) : string 
= let rec aux xs space= 
		let space_new = " "^space in 
  		match xs with
  		| Leaf e -> space^"Leaf "^(pr e)^("\n")
  		| Node (e,lt,rt) -> 
  				(aux lt space_new)^space^"Node "^(pr e)^("\n")
  				 ^(aux rt space_new)
	in aux xs "";;

	(*= failwith "neat tree printer with nested indentation in infix form"*)

let test t =
	print_endline (pr_tree string_of_int t);
	print_endline (pr_tree2 string_of_int t);
	print_endline (pr_tree_infix string_of_int t);;



test t2

(*

	Q7: Numbered List. 

	You have been previously given a printer for lists.

	pr_list2 pr_id ls ==>
	- : string = "[This; is; a; numbered; list]"

	You have been asked to write a list printer that would number each
	element of its list. Your new function pr_list_num  should result in
	the following:

  pr_list_num "; " (fun x->x) ls ==>
  - : string = "[(1)This; (2)is; (3)a; (4)numbered; (5)list]"

  You may use the add_num method below which adds a number to each element
	of its list. You should make use of the |> operator and write it
	in a similar style as pr_list2.
*)
let ( |> ) (x:'a) (f:'a->'b) : 'b =  f x;;

let pr_list2 (pr:'a->string) (xs:'a list) : string
  = "[" ^ (xs 
           |> List.map pr 
           |> String.concat "; ") ^ "]";;

let add_num (xs:'a list) : (int * 'a) list =
  let rec aux xs n =
    match xs with 
    | [] -> []
    | x::xs -> (n,x)::(aux xs (n+1))
  in aux xs 1;;

let ls = ["This";"is";"a";"numbered";"list"];;

let pr_id x = x;;
	
let pr_list_num (sep:string) (pr:'a->string) (xs:'a list) : string
  = "[" ^ (xs 
					 |> add_num
					 |> (fun ys ->
								let rec aux ys=
                  match ys with 
                  | [] -> []
                  | (n,y)::yys -> ("("^(string_of_int n)^")"^y)::(aux yys)
							 in aux ys)
           |> List.map pr 
           |> String.concat "; ") ^ "]";;

let test_num sep pr xs =
  let s = pr_list_num sep pr xs in
  print_endline s;;

(*

  Q8. Higher-order wrappers.

	   These are great for modifying the tracing and monitoring
	   our methods, and could even be user to alter our methods' behaviour.

		 One use of them is to help us perform method call tracing, so
		 as to determine the correctness of our method. A simple tracer for
		 methods is given below which was also applied recursively to the fib method.

		 An example use of tracer is shown below which traced all calls to fib1 4 
		 before returning a final result 5.

# fib1 4;;
fib 0 => 1
fib 1 => 1
fib 2 => 2
fib 1 => 1
fib 0 => 1
fib 1 => 1
fib 2 => 2
fib 3 => 3
fib 4 => 5
- : int = 5

		 Your task is to implement a more selective tracer_test method that
		 takes a predicate, and would only output a call tracing if the predicate
		 holds with the input parameter. An example of its use is below:

     let rec fib3 n =
		   trace_test "fib" (fun x -> x>1) string_of_int string_of_int aux n

		 which only output a trace if input x>1. This would rule out printing
		 the base-cases of x<=1 leading to tracing below:

# fib3 4;;
fib 2 => 2
fib 2 => 2
fib 3 => 3
fib 4 => 5
- : int = 5

     Please modify tracer_test to achieve a more selective tracing of
		 the calls. 

*)

let wrapper (pre:'a->'v) (post:'v->'b->unit)
    (post_exc: 'v->exn->unit) 
    (f:'a->'b) (x:'a) : 'b =
  let v = pre x in
  try 
    let r = f x in
    let () = post v r in
    r
  with e -> 
    let () = post_exc v e in
    raise e;;

let out_print x = print_endline x

(* function tracing *)
let tracer fn_str pr_arg pr_res f x =
  wrapper 
    (fun x -> fn_str^" "^(pr_arg x))
    (fun v r -> out_print (v^" => "^(pr_res r)))
    (fun v e -> out_print (v^" => Exception"))
    f x

(* non-recursive tracing of just the first call *)
let rec fib n = 
  if n<=1 then 1
  else fib (n-1)+(fib(n-2));;

let fib1 n = 
  tracer "fib" string_of_int string_of_int fib n

(* recursive tracing of all calls *)
let rec fib2 n = 
  tracer "fib" string_of_int string_of_int aux n
and aux n = 
  if n<=1 then 1
  else fib2 (n-1)+(fib2(n-2));;

(* selective function tracing *)
let trace_test (fn_str:string) (pr_test:'a->bool) 
    (pr_arg:'a->string) (pr_res:'b->string) (f:'a->'b) (x:'a) : 'b =
  wrapper 
	  (fun x  -> if(pr_test x) then fn_str^" "^(pr_arg x)
							 else "not satisfied") 
    (fun v r -> if(v <>"not satisfied") then out_print (v^" => "^(pr_res r)))
    (fun v e -> out_print (v^" => Exception"))
		f x
		(*failwith "use wrapper to implement a selective function tracing"*)
(* selective tracing of calls *)
let rec fib3 n = 
  trace_test "fib" (fun x -> x>1) string_of_int string_of_int aux n
and aux n = 
  if n<=1 then 1
  else fib3 (n-1)+(fib3(n-2));;


