(* Please see Ocaml String library for relevant methods *)

(* q2 *)
let align_w_penalty s1 s2 penalty_space =
  let n1 = String.length s1 in
  let n2 = String.length s2 in
  let max (m1,s11,s12) (m2,s21,s22)  = 
    if m1<m2 then (m2,s21,s22)
    else (m1,s11,s12) in
  let score a b = if a=b then 1 else -1 in
  let string_of_char c = String.make 1 c in
  let rec aux i j sa sb sum=
    if i>=n1 then
      if j>=n2 then 
        (sum,sa,sb) (* finished traversing both strings *)
      else
        aux i (j+1) (sa^" ") (sb^(string_of_char (String.get s2 j))) (penalty_space + sum) 
    else 
      if j>=n2 then 
        aux (i+1) (j) (sa^(string_of_char (String.get s1 i))) (sb^" ") (penalty_space + sum) 
      else 
        let c1 = String.get s1 i in
        let c2 = String.get s2 j in
        let m = score c1 c2 in
        max (aux (i+1) (j+1) (sa^(string_of_char c1)) (sb^(string_of_char c2)) (m + sum)) (max (aux (i+1) (j) (sa^(string_of_char c1)) (sb^" ") (penalty_space + sum)) (aux i (j+1) (sa^" ") (sb^(string_of_char c2)) (penalty_space + sum)))
  in aux 0 0 "" "" 0


(* q1 *)
let align s1 s2 =
  let penalty = -2 in
align_w_penalty s1 s2 penalty
(* this method finds alignment between two strings
   and print out a best solution *)
	
(* q2 printing *)	
let align_print s1 s2 penalty_space =
  let (m, sa, sb) = align_w_penalty s1 s2 penalty_space in
	print_string  (sa^"\n");
	print_string (sb^"\n");
	for i = 0 to (String.length sa - 1) do
	let c1 = String.get sa i in
        let c2 = String.get sb i in
				if (c1 = ' ' || c2 = ' ') then print_string "*"
				else if(c1 = c2) then print_string "+"
				else print_string "-";
	done;
	print_string ("\nScore : "^(string_of_int m)^"\n");;

(* q3 *)
	class memotable =
		object
		val mutable tb: (int*int*(int * string * string)) list = [];
		val mutable ls: (int*int*(int * string * string)) list = [];
		method add i j x = tb <- (i,j,x)::tb
		method get (i:int) (j:int) =
			ls <- tb;
			let x = ref (1000,"","") in
			while List.length ls > 0 do
			let (a,b,c) = List.hd ls in
			if (a = i && b = j) then x := c;
			ls <- List.tl ls
			done;
			!x
end;;

let align_memostring s1 s2 penalty_space =
	let memotable = new memotable in
  let n1 = String.length s1 in
  let n2 = String.length s2 in
  let score a b = if a=b then 1 else -1 in
  let string_of_char c = String.make 1 c in
  let rec aux i j=
    if i>=n1 then
      if j>=n2 then 
       (* finished traversing both strings *)	
			let result = (0,"","") in
			memotable # add i j result; result
      else
				if let (t,_,_) = memotable # get i j in t = 1000 then
        let (ans,sba,sbb) = aux i (j+1) in
        let x = penalty_space + ans in
				let result = (x, (" "^sba), ((string_of_char (String.get s2 j))^sbb)) in
				memotable # add i j result; result
				else memotable # get i j
    else 
      if j>=n2 then 
				if let (t,_,_) = memotable # get i j in t = 1000 then
        let (ans,sba,sbb) = aux (i+1) (j) in
        let x = penalty_space + ans in
				let result = (x, ((string_of_char (String.get s1 i))^sba), (" "^sbb)) in				
				memotable # add i j result; result
				else  memotable # get i j
      else   
	      if let (t,_,_) = memotable # get i j in t = 1000 then
        let c1 = String.get s1 i in
        let c2 = String.get s2 j in
        let m = score c1 c2 in
				let (a,asa,asb) = aux (i+1) (j+1) in
				let (b,bsa,bsb) = aux (i+1) (j) in
				let (c,csa,csb) = aux i (j+1) in
				let result = 
				if  a + m < b + penalty_space then
					if c + penalty_space < b + penalty_space then (b + penalty_space, (string_of_char c1)^bsa, " "^bsb)
					else (c + penalty_space, " "^csa, (string_of_char c2)^csb)
				else if c + penalty_space < a + m then (a + m, (string_of_char c1)^asa, (string_of_char c2)^asb)
				  else (c + penalty_space, " "^csa, (string_of_char c2)^csb)
				in memotable # add i j result; result
				else memotable # get i j	
	in aux 0 0
	
(* q3 printing *)
let align_print2 s1 s2 penalty_space =
  let (m, sa, sb) = align_memostring s1 s2 penalty_space in
	print_string  (sa^"\n");
	print_string (sb^"\n");
	for i = 0 to (String.length sa - 1) do
	let c1 = String.get sa i in
        let c2 = String.get sb i in
				if (c1 = ' ' || c2 = ' ') then print_string "*"
				else if(c1 = c2) then print_string "+"
				else print_string "-";
	done;
	print_string ("\nScore : "^(string_of_int m)^"\n");;
