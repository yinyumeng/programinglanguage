(*13/14*)

let rec pow x n =
	let rec aux x n res =
		if n =0 then res
		else aux x (n-1) res*x
		in aux x n 1
	
let rec fold_right f xs z = match xs with 
| [] -> z
| x::xs -> f x (fold_right f xs z)

let poly cs x n =
	fold_right (fun a b -> a*(pow x n) +b*x) cs 0
	
let poly cs x n =
	let rec aux xs n res =
		match xs with 
		| []-> res
		| a::rs-> aux rs (n+1) (res + a*(pow x n))
		in aux cs n 0


(*14/15*)
let rec drawLine  n =
	let rec aux n cur = 
  	if n = 1 then 1::cur
  	else 
  		aux  (n-1) (n::cur)
	in aux n []
		
let rec draw n =
	let rec aux n cur index=
		if index<= n then  (drawLine (index+1))::cur
		else
			(drawLine (2*n-1-index))::cur
		in aux n [[]] 0
		
	
	
	