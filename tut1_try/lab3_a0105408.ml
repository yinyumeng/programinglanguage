(* Please see Ocaml String library for relevant methods *)



class ['a,'b] memo_table =
  object (self) 
    val tab = Hashtbl.create 10
    method get (a:'a) =
      try 
        Some(Hashtbl.find tab a)
      with _ -> None
    method store (a:'a) (b:'b) =
      Hashtbl.replace tab a b
  end;;

class ['a,'b] memo_table_ho =
  object (self) 
    inherit ['a,'b] memo_table as super
    method compute (f:'a->'b) (a:'a) : 'b =
      match (self # get a) with
      | None ->
        let r = f a in
        (self # store a r; r)
      | Some v -> v
  end;;

(* ((int,string,string) *)
(*  (int,(string,string)) *)

	

	

(* this method finds alignment where space is given a 
   specific penalty *)
let align_w_penalty s1 s2 penalty_space=
  let n1 = String.length s1 in
  let n2 = String.length s2 in
  let max m1 m2  = 
    if m1<m2 then m2
    else m1 in
  let maxS (m1,a1,a2) (m2,b1,b2)  = 
    if m1<m2 then (m2,b1, b2)
    else (m1,a1,a2) in
	let get_result value las rad (m,a,b) = 
		(value+m,las^a,rad^b) in
  let score a b = if a=b then 1 else -1 in
  let string_of_char c = String.make 1 c in
  let mtab = new memo_table_ho in
  let rec aux i j =
    mtab # compute (fun (i,j) ->
    if i>=n1 then
      if j>=n2 then 
        (0,"","") (* finished traversing both strings *)
      else
				let c2 = String.get s2 j in
        get_result penalty_space " " (string_of_char c2) (aux i (j+1))
    else 
      if j>=n2 then 
				let c1 = String.get s1 i in
        get_result penalty_space (string_of_char c1) " " (aux (i+1) j)
      else 
        let c1 = String.get s1 i in
        let c2 = String.get s2 j in
        let m = score c1 c2 in
        let m1 = get_result m (string_of_char c1) (string_of_char c2) (aux (i+1) (j+1)) in
        let m2 = get_result penalty_space " " (string_of_char c2) (aux i (j+1)) in
        let m3 = get_result penalty_space (string_of_char c1) " " (aux (i+1) j) in
        maxS m1 (maxS m2 m3)) (i,j)
  in aux 0 0 
     
(* you are free to generalise methods below *)
let align s1 s2 =
  let penalty = -2 in  
	align_w_penalty s1 s2 penalty
	  
(* this method finds alignment between two strings
   and print out a best solution *)
let align_print s1 s2 penalty_space =
	let get_sign ac bc = 
		if ac=bc then  "+" 
		else if ac=' ' ||bc=' ' then  "*"
		else  "-" in
	let (m,a,b) = (align_w_penalty s1 s2 penalty_space) in
	print_string (a^"\n"^b^"\n");
	for i = 0 to (String.length a - 1) do
		print_string (get_sign (String.get a i)  (String.get b i))
	done;
	print_string ("\nScore : "^(string_of_int m)^"\n");;


align_print "GATCGGCAT" "CAATGTGAATC" (-1);;

	
(*q1*)