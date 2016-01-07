/*

 Tut 8/Lab 4 : From OCaml to Scala Programming: 
 Submission by 28th Oct 2015 (for Q6-Q11)

 We will try the first few 5 questions during the tutorial,
 and you can complete the lab assignment yourself thereafter.
 Please provide a couple of test cases for each example.
 Please use qn_i for the solution of each of your method.
 For example, your final solution for Q8 should be named qn_8.

*/

object Lab4 extends App {
  println("Hello to Lab4/Tutorial8")
  /* 
  Q1 : Write a recursive function that would return the
       last element of the list. In case of empty list,
       throw a Failure exception.

       What is the polymorphic type of this function?
  */

  def last [X](xs:List[X]) : X 
  = xs match {
    case Nil => throw new Exception("no last element")
    case x::xs => xs match{
        case Nil =>x
        case _ => last(xs)
    }
  }
  println(last(List(1,2,3,4)))

  /* 
  Q2 : Change the last function to one with the following
       type: 'a list -> 'a option
       This function should return Some v, where v is the
       last element of the list. If the list is empty, you
       should return None.
   let last2 (xs: 'a list) : 'a option = 
   failwith "last2 not implemented yet"
  */
  def last2 [X](xs:List[X]) : Option[X] 
    = xs match {
      case Nil => throw new Exception("no last element")
      case x::xs => xs match{
          case Nil =>Some(x)
          case _ => last2(xs)
      }
      
    }
  
  def qn_2[X](xs:List[X])= last2(xs)
  
  println(last2(List(1,2,3,4)))
  println(last2(List("a","b","c")))
  println(last2(List()))
  
  
  def conv (x:Option[String]) :Int = 
    x match{
    case None => -1
    case Some(v) => v.length()
  }
  println( conv(None))
  println( conv(Some("abc")))
  println( conv(Some("")))
  
  def conv_fold_left (x:Option[String]) :Int = 
    x.foldLeft(-1)((b,a)=> a.length)
  println( conv_fold_left(None))
  println( conv_fold_left(Some("abc")))
  println( conv_fold_left(Some("")))
  
/* 
  Q3 : Write a recursive function to sort a list of numbers
       using the insertion sort method.

       For your convenience, we have provided an
       insert procedure.
       (i) can you improve the insert method to
           avoid constructing (y::ys) in the base case?
           (Hint : use the as-pattern notation)
      (ii) implement a recursive sort method

 let rec insert x ys =
  match ys with
    | [] -> [x]
    | y::ys -> 
          if x<=y then x::y::ys
          else y::(insert x ys)
let sort xs=
  failwith "sort method based on insertion sort"

*/
  def insert(x:Int, ys:List[Int]) : List[Int]= 
    ys match{
    case null => throw new Exception("null")
    case Nil => List(x)
    case y::ys => 
      if (x<=y) x::y::ys
      else y::insert(x,ys)
  }
  
  println(insert(2,List(1,3,4,5)))
  println(insert(2,null))
  
  
  def insert_general [A](x:A, ys:List[A]) (implicit ord: math.Ordering[A]): List[A]= 
    ys match{
    case null => throw new Exception("null")
    case Nil => List(x)
    case y::ys => 
      if (ord.lteq(x, y)) x::y::ys
      else y::insert_general(x,ys)
  }
  
  println(insert_general(2,List(1,3,4,5))(scala.math.Ordering.Int))
  println(insert_general("bb",List("aa","abs")))
  println(insert_general(2,null))
  
  
  def sort(xs:List[Int]) : List[Int]=
    xs match{
    case Nil => Nil
    case x::xs => xs match{
      case Nil => insert(x,xs)
      case y::ys =>
        insert(x,sort(xs))
    }
  }
  

/* 
  Q4 : Consider a uprim type to capture either 
       integer, float or a string value.

       You can build a list of mixed type using
       it, and can perform List.rev and List.length
       using it.

       Compute the sum of mixed list using the value_of_mix
       function.
type uprim = I of int | F of float | S of string ;;

let mix_ls = [I 3; F 4.3; S "hello"; I 4];;

print_endline ("mix_ls has length "^(string_of_int (List.length mix_ls)));;
List.rev  mix_ls;;

let value_of_mix up =
  match up with
    | I v -> v
    | F v -> (int_of_float v) (* truncates the float value *)
    | S s -> (String.length s) (* length of string *)

*/
  import math._
  abstract class Tree[X]
  case class Leaf[X](x:X) extends Tree[X]
  case class Node[X](x:X,lt:Tree[X],rt:Tree[X]) extends Tree[X]
  
  
  def max_tree(t:Tree[Int]):Int = 
    t match{
    case Leaf(v) => v
    case Node(v,lt,rt) => max(v,max(max_tree(lt),max_tree(rt)))
  }
  
  Node(2,Leaf(3),Leaf(5))
  println(max_tree(Node(2,Leaf(3),Leaf(5))))


//def insert_general [A](x:A, ys:List[A]) (implicit ord: math.Ordering[A]): List[A]= 
//    ys match{
//    case null => throw new Exception("null")
//    case Nil => List(x)
//    case y::ys => 
//      if (ord.lteq(x, y)) x::y::ys
//      else y::insert_general(x,ys)
//  }

  def max_tree_general[A](t:Tree[A])
                (implicit ord: math.Ordering[A]) :A = 
    t match{
    case Leaf(v) => v
    case Node(v,lt,rt) => ord.max(v,ord.max(max_tree_general(lt),max_tree_general(rt)))
  }
  
  def sum(x:List[Int]) = x.foldLeft(0)(_+_)
  implicit object ListInt extends Ordering[List[Int]]{
    def compare(x:List[Int],y:List[Int]) = 
      sum(x)-sum(y)
  }
  
  
  
  println(max_tree_general(Node(2,Leaf(3),Leaf(5))))
  println(max_tree_general(Leaf(3)))
  println(max_tree_general(Node('a',Leaf('d'),Leaf('h'))))
  println(max_tree_general(Node(List(2),Leaf(List(3,3)),Leaf(List(5)))))
/* 
  Q5 : Consider a polymorphic tree.

       Write a function that will return the largest value in
       the tree. You may use the max function.
type 'aa btree = Leaf of 'aa | Node of 'aa * ('aa btree) * ('aa btree) ;;
let t1 = Leaf 3;;
let t2 = Node(4,t1,t1);;
let t2 = Node(6,t2,t1);;

let rec max_tree (t: int btree) : int =
  failwith "max_tree to be implemented"
*/
  

  


/* 
  Q6 : Below is a function that will flatten a tree into a list
       by traversing the tree in an infix-order.

       Write another function that will flatten a tree in
       based on pre-fix traversal.
let rec flatten_infix (t: 'a btree) : 'a list =
  match t with
    | Leaf v -> [v]
    | Node(v,lt,rt) -> (flatten_infix lt)@[v]@(flatten_infix rt)

let flatten_prefix (t: 'a btree) : 'a list =
  let rec aux t =
    match t with
      | Leaf v -> [v]
      | Node(v,lt,rt) -> failwith "max_tree to be implemented"
  in aux t
*/
 def flatten_infix [X](t:Tree[X]):List[X] = 
    t match{
    case Leaf(v) => List(v)
    case Node(v,lt,rt) => flatten_infix(lt):::List(v):::flatten_infix(rt)
  }
  def flatten_prefix [X](t:Tree[X]):List[X] = 
    t match{
    case Leaf(v) => List(v)
    case Node(v,lt,rt) => flatten_infix(rt):::List(v):::flatten_infix(lt)
  }
  
  def qn_6[X](t:Tree[X])= flatten_prefix(t)
  
  println(qn_6((Node(2,Leaf(3),Leaf(5)))))
  println(qn_6(Leaf(3)))
  println(qn_6(Node('a',Leaf('d'),Leaf('h'))))
  println(qn_6(Node(List(2),Leaf(List(3,3)),Leaf(List(5)))))

/* 
  Q7 : 

       The above code below merely expresses the fact that
         power x 0 = 1
         power x n = x * (power (n-1))

       The above function is NOT tail-recursive.
       Can you write a tail-recursive
       version of this function which would accumulate its
       result in a 3rd paramater, called acc?

let power2 (x:int) (n:int) : int =
  let rec aux x n acc = 
    failwith "power2 is yet to be implemented"
  in aux x n 1
*/
  def power2 (x: Int) (n: Int): Int ={
    if (x==0)  1
    else x*(power2(x)(n-1))
  }


/* 
  Q8 : 

       We can also get a logarithmic-time function using

         power x 0 = 1
         power x (2*n = power (x^2) n
         power x (2*n+1) = x*(power (x^2) n)

       Implement such a function tail-recursively.
       How does this compare with the cryptic version of the code
       shown in Lecture 1.

 let power3 (x:int) (n:int) : int =
  let rec aux x n acc = 
    failwith "power3 is yet to be implemented"
  in aux x n 1

*/

def power3 (x: Int) (n: Int): Int ={
    if (x==0)  1
    else if(n%2==0) (power3(x*2)(n/2))
    else x*(power3(x*2)((n-1)/2))
  }
/* 
  Q9: Last via List.fold_Left
	Consider the last function after.
	Re-implement it using fold_left.
*/

/* 
  Q10: You can compute the average of a list of
	numbers by dividing the sum of the elements by
	the length of the list. Use a single fold_left to
	compute both these values, and then compute
	the average. Throw an exception if the list is empty.

(* replace failwith by your code *)
let average (xs: int list) : float =
	failwith "average to be computed with a single fold_left"

*/


def average (xs: List[Int]): Float =
  def conv_fold_left (x:Option[String]) :Int = 
    xs.foldLeft(-1)((b,a)=> a.length)

/* 
  Q11 : Higher-Order functions for Trees

	You have designed a new tree data structure.
	It is a good practice	to provide a set of higher-order functions.

	(i) Based on your understanding of List.map, implement
		  a corresponding version for the map_tree function. 

	(ii) Similarly, based on your understanding of List9.fold_right, implement
		   a corresponding version for the fold_tree function. 

	Some examples of their uses are given below. They may be
	used as test cases for your implementation.


type 'a tree = 
	| Leaf of 'a
	| Node of 'a * ('a tree) * ('a tree);;

let t1 = Node (3,Leaf 1, Leaf 2);;
let t2 = Node (4,t1,t1);;
let t3 = Node (5,t2,t1);;

let rec map_tree (f:'a -> 'b) (t:'a tree) : 'b tree =
	failwith "to map a function to each node and leave"
(* 
   map_tree f (Node a1,Leaf a2,Leaf a3) 
    ==> Node (f a1, Leaf (f a2), Leaf (f a3))
*)

let fold_tree (f1:'a->'b) (f2:'a->'b->'b->'b) (t:'a tree) : 'b =
	failwith "to reduce a tree with f1,f2 to a value of output 'b type"
(* 
   fold_tree f1 f2 (Node a1,Leaf a2,Leaf a3) 
    ==> f2 a2 (f1 a1) (f1 a1)
*)

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
*/

}
