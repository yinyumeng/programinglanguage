let reverse (xs:'a list) : 'a list =
  List.fold_left (fun acc x -> x::acc) ([]) xs;;

let product xs ys = 
  List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	
let product_wo_concat xs ys = 
	List.map (fun x -> List.map (fun y -> (x,y)) ys) xs

let product_column_major xs ys = 
	List.map (fun y -> List.map (fun x -> (x,y)) xs) ys

let product2 xs ys = 
	let res = List.concat (List.map (fun x -> List.map (fun y -> (x,y)) ys) xs)
	in List.filter(fun(x,y) -> x<=y) res


let divisor_prod xs ys = 
	List.concat (List.map (fun y -> List.map (fun x -> (x/y))  xs)(List.filter(fun y -> y!=0) ys))
  
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
	
type 'a tree = 
  | Leaf of 'a
  | Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,Leaf 6);;
let t3 = Node (5,t2,Leaf 3);;


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
	
let add_n (t:int tree) (n:int) : int tree = 
	let rec aux t1 n =
		fold_tree
  	(fun a ->  Leaf (a+n))
  	(fun a tl tr ->
			 Node(a+n, tl ,tr )) t1 in aux t n


let right_most (t:'a tree) : 'a = 
  fold_tree
  	(fun a -> a)
  	(fun a r1 rr -> rr)
  	t
		
		
let mirror_tree (t:'a tree)  = 
  fold_tree
  	(fun a ->  Leaf (a))
  	(fun a tl tr ->
			 Node(a, tr,tl))
  	t

let t4 = Node ('a',Leaf 'b',Node ('c',Leaf 'e',Leaf 'f'));;

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

let t5 = Node(2,Leaf 1, Leaf 3);;

let check_bst t =
	snd(fold_tree
  	(fun a -> (a,true))
  	(fun a (ll,tl) (lr,tr) ->
			(a,(ll<a && a<=lr && tl && tr)))
  	t)
		
let fold_tree_preorder (f:'z->'a->'z) (z:'z) (t:'a tree) : 'z = 
  let rec aux z t = match t with
    | Leaf a -> f z a
    | Node (a,lt,rt) -> 
      let z1 = f z a in
      let z2 = aux z1 lt in
      aux z2 rt
  in aux z t;;	
		
let count_tree (t:'a tree) : int =  
	fold_tree_preorder
		(fun acc a-> acc+1)
  	0 t
		
let pr_tree (pr:'a->string) (xs:'a tree) : string 
 = let rec aux xs = 
		match xs with
		| Leaf e -> "Leaf "^(pr e)^("\n")
		| Node (e,lt,rt) -> 
				"Node "^(pr e)^("\n")
				 ^(aux lt)^(aux rt)
	in aux xs;;


let pr_tree2 (pr:'a->string) (xs:'a tree) : string 
= let rec aux xs space= 
		let space_new = " " ^space in 
  		match xs with
  		| Leaf e -> space^"Leaf "^(pr e)^("\n")
  		| Node (e,lt,rt) -> 
  				space^"Node "^(pr e)^("\n")
  				 ^(aux lt space_new)^(aux rt space_new)
	in aux xs "";;


let pr_tree_infix (pr:'a->string) (xs:'a tree) : string 
= let rec aux xs space= 
		let space_new = " "^space in 
  		match xs with
  		| Leaf e -> space^"Leaf "^(pr e)^("\n")
  		| Node (e,lt,rt) -> 
  				(aux lt space_new)^space^"Node "^(pr e)^("\n")
  				 ^(aux rt space_new)
	in aux xs "";;


let test t =
	print_endline (pr_tree string_of_int t);
	print_endline (pr_tree2 string_of_int t);
	print_endline (pr_tree_infix string_of_int t);;		
		


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

let tracer fn_str pr_arg pr_res f x =
  wrapper 
    (fun x -> fn_str^" "^(pr_arg x))
    (fun v r -> out_print (v^" => "^(pr_res r)))
    (fun v e -> out_print (v^" => Exception"))
    f x

let rec fib n = 
  if n<=1 then 1
  else fib (n-1)+(fib(n-2));;

let fib1 n = 
  tracer "fib" string_of_int string_of_int fib n

let rec fib2 n = 
  tracer "fib" string_of_int string_of_int aux n
and aux n = 
  if n<=1 then 1
  else fib2 (n-1)+(fib2(n-2));;

let trace_test (fn_str:string) (pr_test:'a->bool) 
    (pr_arg:'a->string) (pr_res:'b->string) (f:'a->'b) (x:'a) : 'b =
  wrapper 
	  (fun x  -> if(pr_test x) then fn_str^" "^(pr_arg x)
							 else "not satisfied") 
    (fun v r -> if(v <>"not satisfied") then out_print (v^" => "^(pr_res r)))
    (fun v e -> out_print (v^" => Exception"))
		f x
	failwith "use wrapper to implement a selective function tracing"

let rec fib3 n = 
  trace_test "fib" (fun x -> x>1) string_of_int string_of_int aux n
and aux n = 
  if n<=1 then 1
  else fib3 (n-1)+(fib3(n-2));;


let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
  let rec aux t =
    match t with
      | Leaf x -> f1 x
      | Node (x,lt,rt) -> f2 x (aux lt) (aux rt) 
  in aux t

let fold_tree_preorder (f:'z->'a->'z) (z:'z) (t:'a tree) : 'z = 
  let rec aux z t = match t with
    | Leaf a -> f z a
    | Node (a,lt,rt) -> 
      let z1 = f z a in
      let z2 = aux z1 lt in
      aux z2 rt
  in aux z t;;

let fold_tree_preorder_use_fold_tree (f:'z->'a->'z) (z:'z) (t:'a tree) : 'z = 
  