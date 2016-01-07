(* 
   Tutorial 1 : Introduction to OCaml
   
   Week of 24th August 2015

   Use: ocamlc -annot tut1.ml
   (to compile into an executable a.out, and a type annotation file)
   
   Reference on Ocaml (Real-World OCaml):
      https://realworldocaml.org/

   Emacs cheatsheet with Ocaml mode:
      https://www.lri.fr/~conchon/IPF/fiches/tuareg-mode.pdf
*)

(* 
 Q1. Consider the expression below
*)
 
  let x = 2 in
  let y = 3 in
  let x = x * 4 in
  print_endline ("Q1 Ans = "^(string_of_int (x+y))) ;;
  

(* Which x is being referenced in the last line?*)
(* 4*x*)
(* 11
   What output will be printed? *)

(*
Q2. Consider the expression below
*)
 
  let x = 2 in
  let y = let x = x * 4 in 3 in
	(* let y = (let x = x * 4) in 3 in*)
	let y = (let x = x * 4 in 3 + x)in
  print_endline ("Q2 Ans = "^(string_of_int (y))) ;;


(* Which x is being referenced in the last line?*)
(* 2*)
(* 5
   What output will be printed? *)
  
(* Q3. Consider the function below *)
 
  let foo x = x+1
	(*val foo : int ->int = <fun>*)

(* What is the type of this function? *)

(* Q4. Consider the function below *)
 
  let goo (x,y) = x
	

   
(* What is the type of this function? *)


(* Q5. Consider the function below *)
 
  let hoo x y = x
	let pf = hoo "hello";;
  pf 1;;
	
	(*first one after pf will decide the type of the parameter so tha if the first*)
	(* one typed is int, then the next will always be int*)
	(* hoo = *)
	
	(*foo x y*)
	(* foo x == fun y -> goo(x, y)*)
	(* *)
	(* hoo 3 4*)
	(* ((hoo 3) 4)*)
   
(*  
    What is the type of this function? 
	Does this function has the same type as ggo?
	How are the two functions related?
*)


(* Q6. Consider the maxlist function below *)
 
   let rec maxlist xs =
		(*
			 let rec maxlist (xs : int list) =
		*)
     match xs with
	 | [] -> failwith "empty list"
	 | [x] -> x
	 | x::ys -> 
		let m2 = maxlist ys in
		if x>m2 then x
		else m2 ;;
		
   print_endline ("Q6 Ans (maxlist [3;16;1] = "^(string_of_int (maxlist [3;16;1]))) ;;
   print_endline ("hello");;

(* 
	(i) What is the type of this function?
	(ii) Explain what happen when an empty list is supplied as the input?
    (iii) Is it possible for recursive maxlist ys call to throw an exception?	
		no the third case has two or more values so that ys has one or more. can not be empty
*)

(*
  Q7 : Rewrite maxlist function to use an auxiliary recursive
       function, as follows:
*)

  let maxlist2 xs =
     let rec aux xs sofar =
		match xs with
		| [] -> sofar
		| x::ys -> 
			if x > sofar then aux ys x
			else aux ys sofar
			(*failwith "to be completed"*)
	in match xs with
	| [] -> failwith "empty list"
	| x::ys -> aux ys x

(*
   How is this function different from the version in Q6
   Is it any better?
*)

(*
  Q8 : The maxlist function currently has type: int list -> int.

  Rewrite it to a version that is based on the following option
  type from the Pervasive standard library:

     type 'a option = None | Some of 'a
*)


let maxlist2 xs =
    let rec aux xs sofar =
	
		match xs with
		| [] -> sofar
		| x::ys -> 
			if x > sofar then aux ys x
			else aux ys sofar
	
	in match xs with
	| [] -> None
	| x::ys ->Some( aux ys x)
	(*option value: None and Some*)
	(*pr_option (fun a -> string)*)
	
			(*failwith "to be completed"*)
	(*in match xs with
	| [] -> failwith "empty list"
	| x::ys -> aux ys x
	
	
	pr_option f opt =
		match opt with 
		| None -> "None"
		| Some a -> "Some " ^(f a)
		*)

	
	

  let maxlist3 (xs:int list) : int option =
    failwith "to be completed"
