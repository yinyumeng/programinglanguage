/*


  Tutorial 7 : 12th October 2015 (Scala Programming) 

Q1. Discuss how higher-order functions are supported
    the Scala language.

Q2. Explain why static fields and static methods are not being
    supported by Scala. Do you lose any expressivity?

Q3. Consider a data structure below to support the abstract syntax tree
    of lambda calculus syntax 

 abstract class OTerm {
 class OVar(name: String) extends OTerm {
 }
 class OLam(arg: String, body: OTerm) extends OTerm {
 }
 class OApp(f: OTerm, v: OTerm) extends OTerm {
 }

    Implement the toString method that would allow expressions of the
    lambda calculus to be pretty-printed.

Q4. Design a set of functions to find the free variables of lambda
    terms for both functional and OO data structures in the code
    fragment below.

Compilation & Execution Instruction
===================================
  To compile this file, use either
         (i) scalac tut7.scala
     or (ii) fsc tut7.scala
  To run this class file, please use use:
     scala Tut7

  Note that fsc is a fast scala compiler which runs a
  compilation daemon in the background, to avoid the overheads
  of re-starting the compiler.
*/

abstract class OTerm {
  def free_vars : List[String] = 
    List()//throw new Exception("not yet implemented")
}
class OVar(name: String) extends OTerm {
  override def toString = name
  override def free_vars = List(name)
}
class OLam(arg: String, body: OTerm) extends OTerm {
  override def toString = "\\" ++ arg ++ " . " ++ body.toString
  override def free_vars = List()
}
class OApp(f: OTerm, v: OTerm) extends OTerm {
  override def toString = "(" ++ f.toString ++ " " ++ v.toString ++")"
  override def free_vars = List()
}

abstract class Term
case class Var(name: String) extends Term {
  override def toString = name
}
case class Fun(arg: String, body: Term) extends Term {
  override def toString = "\\" ++ arg ++ " . " ++ body.toString
}
case class FApp(f: Term, v: Term) extends Term {
  override def toString = "(" + f + " " ++ v.toString ++")"
}

object Tut7 extends App {
  // implicit def oTermtoString (x: OTerm) : String 
  // = x.toString
  println("Hello World")
  val t1 = new OLam("x", new OVar("x"))
  val t2 = new OApp(new OVar("x"), new OVar("y"))
  val t3 = new OApp(new OVar("z"), t2)
  println(t1)
  println(t3)
  val f1 = Fun("x", Var("x"))
  val f2 = FApp(Var("x"), Var("y"))
  val f3 = FApp(Var("z"), f2)
  val f4 = Fun("x", f2)
  def toStr (t:Term) : String = {
    t match {
      case Var(n) => n // TBI
      case Fun(arg,body) => {
        "\\" ++ arg ++ " . " ++ toStr(body)
      }
      case FApp(t1,t2) => "(" ++ toStr(t1) ++ " " ++ toStr(t2) ++")"

    }
  }
  println(f1)
  println(f3)
  println(toStr(f1))
  println(toStr(f3))
  def free_vars (t:Term) : List[String] = {
    t match {
      case Var(n) => List(n)
      case Fun(arg,body) => {
        free_vars(body).filter(x => !(x==arg))
      }
      case FApp(t1,t2) => 
        (free_vars(t1) ++ free_vars(t2)).distinct
    }
  }
 println(free_vars(f1))
 println(free_vars(f3))
 println(free_vars(f4))
 println(t1.free_vars)
 println(t3.free_vars)
}
