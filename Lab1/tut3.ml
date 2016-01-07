(*******************************************)
(* Lab/Tutorial 3 : Higher-Order Functions *)
(*******************************************)
(*  Week of 7 Sept *)

(* 
  Q1: Last via List.fold_Left
	
	Consider the last function below.
	Re-implement it using fold_left.
*)

let last (xs:'a list) : 'a =
	let rec aux xs prev =
		match xs with
		| [] -> prev
		| x::ys -> aux ys x in
	match xs with
	| [] -> failwith "no last element"
	| x::xs -> aux xs x


let last (xs:'a list) : 'a =
	let rec aux xs prev =
		match xs with
		| [] -> prev
		| x::ys -> aux ys x in
	match xs with
	| [] -> failwith "no last element"
	| x::xs -> aux xs x
(* replace failwith by your code *)
let last2 (xs:'a list) : 'a =
	failwith "to be implemented using List.fold_left"

(* 
  Q2 : Sorting
	
	Consider the insertion sort method below.	
	Re-implement the two methods using List.fold_right.
*)

let rec insert x ys =
  match ys with
    | [] -> [x]
    | y::ys -> 
          if x<=y then x::y::ys
          else y::(insert x ys);;

let rec sort xs =
	match xs with
	| [] -> []
	| y::ys -> insert y (sort ys);;

(* replace failwith by your code *)
let insert2 x ys =
	
	failwith "to be implemented using List.fold_right"

let sort2 xs =
	failwith "to be implemented using List.fold_right"

(*sum the every other value*)
(*f z y = if flag then (false, y+sum)
				else (true, sum)*)
(*flag current_sum y*)	
(* 
  Q3 : You can compute the average of a list of
	numbers by dividing the sum of the elements by
	the length of the list. Use a single fold_left to
	compute both these values, and then compute
	the average. Throw an exception if the list is empty.
*)

(* replace failwith by your code *)
let average (xs: int list) : float =
	let s = List.fold_left (+) (0) xs in
	let len = List.fold_left (fun z x -> z+1) (0) xs in
	(float_of_int s)/.(float_of_int len)
	
let average (xs: int list) : float =
	let (s,len) = List.fold_left 
		(fun (s_rec,l_rec) x-> (x+s_rec, 1+l_rec)) (0,0) xs in
	(float_of_int s)/.(float_of_int len)
	
	 
	failwith "average to be computed with a single fold_left"

(* 
  Q4 : Using Pipeline
	
	You can compute the median of a list of
	integers by sorting the list and computing its
	length, and then finding a middle element in
	the list. If there is an even number of elements,
	you are expected to compute the average of middle two
	elements. You are to use the |> operator as
	below in your median method.
*)

(*this one is not used in general library*)
let ( |> ) (x:'a) (f:'a->'b) : 'b =  f x;;

(* your implementation for mid need not use higher-order functions *)
let mid (xs:int list) : float =
	(* pre: input list is sorted *)
	xs 
	|> List.length
	|> (fun n -> 
		let mid = n/2 in
		if n mod 2 ==0 then 
			let r1 = List.nth xs (mid -1)in
			let r1 = List.nth xs (mid) in
			(float_of_int (r1+r2))/.2.0
			else
				let r1 = List.nth xs (mid -1)in
				(float_of_int r2)
				)
		
let median xs =
	xs 
	|> sort	
	|> mid

let median xs = mid(sort xs);;
let median xs = 
	let r1 = (sort xs)
	in mid r1

(* 
  Q5 : Higher-Order functions for Trees
	
	You have designed a new tree data structure.
	It is a good practice	to provide a set of higher-order functions.
	
	(i) Based on your understanding of List.map, implement
		  a corresponding version for the map_tree function. 

	(ii) Similarly, based on your understanding of List9.fold_right, implement
		   a corresponding version for the fold_tree function. 
		
	Some examples of their uses are given below. They may be
	used as test cases for your implementation.
	 
*)

type 'a tree = 
	| Leaf of 'a
	| Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,t1);;
let t3 = Node (5,t2,t1);;

let rec map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
	let rec aux t =
		match t with
		| Leaf x -> Leaf (f x)
		|	Node (x, lt, rt) -> Node (f x,aux lt,aux rt )
		in (aux t);;
	failwith "to map a function to each node and leave"
	
map_tree (fun x -> -22) t2;;
(* 
   map_tree f (Node a1,Leaf a2,Leaf a3) 
    ==> Node (f a1, Leaf (f a2), Leaf (f a3))
*)

let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
	let rec aux t = 
		match t with
		| Leaf x -> (f1 x)
		| Node (x, lt, rt) -> (f2 x (aux lt) (aux rt))
		in (aux t);;
	
	failwith "to reduce a tree with f1,f2 to a value of output 'b type"
	
(* 
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3) 
    ==> f2 a2 (f1 a1) (f1 a1)
*)
	
let sum_tree t = fold_tree (fun a -> a) (fun a r_1 r_r -> a+r_1+r_r) t
let count_tree t = fold_tree (fun a -> 1) (fun a r_1 r_r -> a+r_1+r_r) t
let count_leaves t = fold_tree (fun a -> 1) (fun a r_1 r_r -> r_1+r_r) t
let height t = fold_tree( fun a ->1)
		(fun a r_1 r_r -> 1+(max r_1 r_r))

let t4=map_tree (fun x -> 2*x) t3;;
(* expecting a doubled version of t3
   Node (10, Node (8, Node (6, Leaf 2, Leaf 4), Node (6, Leaf 2, Leaf 4)),
     Node (6, Leaf 2, Leaf 4))
*)
fold_tree (fun x -> x) (fun a b c -> a+b+c) t3;;
(* expecting 27 *)
fold_tree (fun x -> [x]) (fun a b c -> b@(a::c)) t1;;
(* in-order traversal [1; 3; 2] *)
fold_tree (fun x -> [x]) (fun a b c -> a::(b@c)) t1;;
(* pre-order traversal [3; 1; 3] *)

fold_tree (fun x -> [x]) (fun a b c -> (b@c)@[a]) t1;;


(* post-order traversal [1;2;3] *)
(*::conbine one value and a list*)
(* @ combine two lists*)

let pick_leaves t = fold_tree (fun x -> [x])
(fun a b c -> b@c) t
(* 
  Q6 : Writing one higher-order function in terms of another.
	
	Implement List.map, List.filter and List.partition
        in terms of LIst.fold_right.

	 
*)
let map_tree f t = fold_tree (fun a -> Leaf(f a) )(fun a l_rec r_rec -> Node(f a, l_rec, r_rec)) t


let partition1 pred xs = 
	List.fold_right 
		(fun x (lst1,lst2)->
			if pred x then (x::lst1,lst2)
			else  (lst1,x::lst2)) 
		xs ([],[])
		
let partition2 pred xs = 
	List.fold_left 
		(fun  (lst1,lst2) x->
			if pred x then (lst1@[x],lst2)
			else  (lst1,lst2@[x])) 
		 ([],[]) xs
		
		
partition1(fun x-> x mod 2==0) [2;3;4];;