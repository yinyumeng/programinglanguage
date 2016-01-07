/*
  Lab 5 : 2nd Nov 2015

Compilation & Execution Instruction
===================================
  To compile this file, use either
         (i) scalac lab5b.scala
     or (ii) fsc lab5b.scala
  To run this class file, please use use:
     scala Lab5

  Note that fsc is a fast scala compiler which runs a
  compilation daemon in the background, to avoid the overheads
  of re-starting the compiler.

 Please use this compiler version or later:
 
 Scala code runner version 2.10.2 -- Copyright 2002-2013, LAMP/EPFL

IMPORTANT
=========
This version contains an almost complete lexer and
a partial parser. The issues you need to resolve are:
 (i) lexer - "let" and "in" should be marked as Sym(..) rather than Id(..)
 (ii) parser - lambda abstraction should allow multiple parameters
         - let construct must be supported
Happy exploration with this parser.
Please do "rm *.class" if needed to clear previous compiled files.
*/
case class parseErr(smth:String)  extends Exception
case class lexErr(smth:String)  extends Exception
case class endOfInput(smth:String)  extends Exception
case class toBeImplemented(smth:String)  extends Exception

object ParserC {

  type Parser[T,A] = List[T] => (A,List[T])

  def seq[T,A,B](p1:Parser[T,A],p2:Parser[T,B]) 
              (toks:List[T]) : ((A,B),List[T]) = {
    val (t1,tok1) = p1(toks)
    val (t2,tok2) = p2(tok1)
    ((t1,t2),tok2)
  }
  def alt[T,A,B](p1:Parser[T,A],p2:Parser[T,A]) 
              (toks:List[T]) : (A,List[T]) = {
    try {
      p1(toks)
    } catch {
      case parseErr(s) => p2(toks)
    }
  }
  def map[T,A,B](p:Parser[T,A],fn:A=>B) (toks:List[T]) : (B,List[T])
  = {
        val (t1,tok1) = p(toks)
        (fn(t1),tok1)
  }
  def empty[T,A](toks:List[T]) :(List[A],List[T]) = {
    (Nil:List[A],toks)
  }
  def opt[T,A](p:Parser[T,A]) 
              (toks:List[T]) : (Option[A],List[T]) = {
    alt(map(p,{(x:A) => Some(x)})
      ,map(empty[T,A],{(x:List[A]) => None})) (toks)
  }
  def rep[T,A](p:Parser[T,A]) 
              (toks:List[T]) : (List[A],List[T]) = {
    def foo (t:(A,List[A])) = { t match {
                                case (a,b) => a::b
                              }}
    alt(map(seq(p,rep(p)),foo)
       //{(a:A,b:List[A]) => a::b})
        ,empty[T,A]) (toks)
  }
  def rep1[T,A](p:Parser[T,A]) 
              (toks:List[T]) : (List[A],List[T]) = {
    def foo (t:(A,List[A])) = { t match {
                                case (a,b) => a::b
                              }}
    map(seq(p,rep(p)),foo) (toks) 
  }
  def explode (s:String) : List[Char] = {
    (s :\ (Nil:List[Char])) {(c,acc) => c::acc}  
  }

  def implode (lst:List[Char]) : String = {
    (lst :\ ("")) {(c,s) => c+s}  
  }
}



object Lab5 extends App {
  // implicit def oTermtoString (x: OTerm) : String 
  // = x.toString
  def loggingL(x:String) = {
//    println(x)
  }
  // logger for Parser
  def loggingP(x:String) = {
    println(x)
  }
  import ParserC._


// You may use the following abstract syntax tree from Tut7

abstract class Term
case class Var(name: String) extends Term {
  //override def toString = name
}
case class Fun(arg: String, body: Term) extends Term {
  //override def toString = "\\" ++ arg ++ "." ++ body.toString
}
case class FApp(f: Term, v: Term) extends Term {
  //override def toString = "("++f.toString ++ " " ++ v.toString++")"
}
case class Let(n: String,t1: Term, t2: Term) extends Term {
  //override def toString = "(let "++n.toString ++ "=" ++ t1.toString++
  //    " in "++t2.toString ++")"
}

abstract class Token
case class Sym(name: String) extends Token
case class Id(name: String) extends Token

def free_vars (t:Term) : List[String] =
    t match {
      case Var(n) => List(n)
      case Fun(arg,body) => {
        val vs = free_vars(body)
        vs filterNot (x => (x==arg))
      }
      case FApp(t1,t2) => free_vars(t1) ++ free_vars(t2)
      case Let(v,t1,t2) => {
         val vs = free_vars(t1)
         (vs filterNot (x => (x==v))) ++ free_vars(t2)
      }
    }


def lex_sym(xs:List[Char]) : (Option[String],List[Char]) = {
  val ys = remove_spaces(xs)
  ys match {
    case Nil => throw new endOfInput("in lex_sym")
    case c::cs => 
      if (c == '(') (Some("("),cs)
      else if (c==')') (Some(")"),cs)
      else if (c=='.') (Some("."),cs)
      else if (c=='=') (Some("="),cs)
      else if (c=='\\') (Some("\\"),cs)
      else {
//        loggingL("Err in lex_sym:("+c+")")
        (None,ys)
      }
  }
}

def is_space(x:Char) =  ((x==' ') || (x=='\t') || (x=='\n') ) 

def is_letter (c:Char) = 
  (('a'<=c && c<='z') || ('A'<=c && c<='Z') || c=='_')

def is_digit (c:Char) = ('0'<=c && c<='9') 

def remove_spaces(xs:List[Char]) : List[Char] =
  {
  xs match {
    case Nil => Nil
    case c::cs => 
      if (is_space(c)) remove_spaces(cs)
      else xs
  }
}


def alpha_num(xs:List[Char]) : (Option[String],List[Char]) =
  {
  // picks rest of alphanumeric chars to form a word
   def aux1(xs:List[Char],acc:List[Char]) :(Option[String],List[Char]) = {
     xs match {
       case Nil => (Some(implode(acc.reverse)),xs)
       case c::cs => 
         if (is_letter(c) || is_digit(c)) aux1(cs,c::acc)
         else (Some(implode(acc.reverse)),
                 xs)
     }
   }
  // picks first alphabetic letter
  val ys = remove_spaces(xs)
  ys match {
    case Nil => throw new endOfInput("in alphaNum")
    case c::cs => 
      if (is_letter(c)) aux1(cs,List(c))
      else {
        loggingL("Unrecogized char:("+c+")")
        (None,ys)
      }
   }
}


def lex_id(xs:List[Char]) : (Option[Token],List[Char]) = 
  {
  val (id,rest) = alpha_num(remove_spaces(xs))
  loggingL("alphanum:"+id)
  id match {
    case None => (None,rest)
    case Some(s) => {
      loggingL("inside lex_id "+s)
//      loggingL("TODO:must cater for let/in reserved symbol")
      if (s=="let") (Some(Sym(s)),rest)
      else if (s=="in") (Some(Sym(s)),rest)
      else{
      (Some(Id(s)),rest)
      }
    }
  }
}

def lex (xs:String):List[Token] =
  {
     val lst = explode(xs)
     //loggingL("Input:"+lst)
     def aux(xs:List[Char]):List[Token] = {
       try {
         if (xs==Nil) List()
         else {
           val (ans,rest) = lex_sym(xs)
           //loggingL("sym:"+ans)
           ans match {
             case None => {
               val (tok,rest2) = lex_id(xs)
               tok match {
                 case None => 
                   if (rest2==Nil) return Nil
                   else throw new lexErr("unrecognized:"+implode(xs))
                 case Some(i) => i::aux(rest2)
               }
             }
             case Some(s) => Sym(s)::aux(rest)
           }
         }
       } catch {
         case endOfInput(msg) => List()
       }
     }
  aux(lst)
}

// parser for identifier
def parse_id (xs:List[Token]):(String,List[Token]) =
  {
  xs match {
    case Id(s)::rest => (s,rest)
      case _ => throw new parseErr("Id not found")
  }
}

// parser for variable into Term
def parse_var (toks:List[Token]):(Term,List[Token]) = 
  map(parse_id, (s:String) => Var(s)) (toks)

// parser reserved symbol k
def parse_sym(k:String) (xs:List[Token]):(Unit,List[Token]) =
  {
  val sym_not_found = new parseErr("Sym "+k+" expected")
  xs match {
    case Sym(s)::rest => if (k==s) ((),rest) else throw sym_not_found
      case _ => throw sym_not_found
  }
}

// parser for one or more terms to denote nested FApp terms
// e1 e2 e3 ==> FApp((FApp(e1,e2),e3)
def parse_apps (toks:List[Token]):(Term,List[Token]) =
  {
  map(rep1(parse_factor),(ts:List[Term]) => 
    {ts match {
      case t::rest => {
        // below is a fold_left over list of terms
        (t /: rest) ((t1,t2) => FApp(t1,t2))
      }
      case Nil => 
        {
          loggingP("parse_apps:empty")
          throw new parseErr("empty applications")
        }
    }}
  ) (toks)
}

// / x . t
def parse_fun (toks:List[Token]):(Term,List[Token]) = {
//  def extr (x:(Unit,(String,(Unit,Term)))):Term = {
  def extr (x:(Unit,(List[String],(Unit,Term)))):Term = {
    x match {
//      case (_,(id,(_,body))) => Fun(id,body)
      case (_,(lst,(_,body))) => (lst :\ body) (Fun) // Fun(lst.head,body)//
    }
  }
//  map(seq(parse_sym("\\"),seq(parse_id,seq(parse_sym("."),parse_term))),extr)(toks)
  map(seq(parse_sym("\\"),seq(rep1(parse_id),seq(parse_sym("."),parse_term))),extr)(toks)
// throw new parseErr("to be implemented ..")
}

// parser for (1)let (2)v (3)= (4)t1 (5)in (6)t2
def parse_let (toks:List[Token]):(Term,List[Token]) ={
  def extr (x:(Unit,(String,(Unit,(Term,(Unit,Term)))))):Term = {
    x match {
//      case (_,(id,(_,body))) => Fun(id,body)
      case (_,(id,(_,(term1,(_,term2))))) => Let(id,term1,term2)//(lst :\ body) (Fun) //
    }
  }
  map(seq(parse_sym("let"), seq(parse_id, seq(parse_sym("="), seq(parse_term, seq(parse_sym("in"), parse_term))))), extr)(toks)
}
  


// parser for (..)
def parse_bracket (toks:List[Token]):(Term,List[Token]) = {
  def extr(x:(Unit,(Term,Unit))):Term = {
    x match {
      case (_,(v,_)) => v
    }
  }
  map(seq(parse_sym("("),seq(parse_term,parse_sym(")"))),extr)(toks)
}
// throw new parseErr("expecting lambda term")

// consider also:
// \ x . t | let v=t1 in t2 | (...)
// Note parse_var need to be placed last
def parse_factor(toks:List[Token]):(Term,List[Token]) = {
  alt(parse_fun,alt(parse_let,alt(parse_bracket,parse_var))) (toks)
}

// parser for term
// consider also:
// \ x . t | let v=t1 in t2 | (...)
// Note parse_var need to be placed last
def parse_term(toks:List[Token]):(Term,List[Token]) =
  { parse_apps (toks)
}
//throw new toBeImplemented("parser for lambda term")

def parse_lambda(x:String):Option[Term] = {
  val toks = lex(x)
  try {
        val (b,remaintoks) = parse_term(toks)
        remaintoks match {
          case Nil => Some(b)
          case _ => {
            loggingP ("Parse Error(unfinished input):"+remaintoks)
            None
          }
        }
  } catch {
        case parseErr(s) => {
            loggingP("Parse Error:"+toks)
            None
          }
  }
}

//
//  def eval_to_value(x:Term):Term = {
//    x match {
//      case Var(_) =>x
//      case Fun(lt,rt) => x
//      
//      case FApp(t1,t2) =>{
//        val nt1 = eval_to_value(t1) 
//        val nt2 = eval_to_value(t2) 
//        nt1 match {
//          case Fun(y,body) => eval(subst())
//          case _ => FApp(nt1,nt2)
//        }
//      }
//    }
//  }
//
//  def subst(v:String, nt:Term,t2:Term):Term = {
//    t2 match {
//      case Var(w) =>{
//        if(v==w)  nt
//        else t2
//      }
//      case Fun(y,body)=>
//        if 
//    }
//  }
  def eval_to_value(x:Term):Term = {
    
    val t = change_let_in(x)
    val t1 = rename_process(t)
    reduce(t1)
  }
  
  def reduce(x:Term): Term = {
    x match {
      case FApp(Fun(st,Var(lv)),rv) => {
        if(lv==st) reduce(rv)
        else{
          Var(lv)
        }
      }
      case FApp(Fun(st,FApp(lt,rt)),rv) => {
        reduce(FApp(reduce(FApp(Fun(st,lt),rv)),reduce(FApp(Fun(st,rt),rv))))
      }
      case FApp(Fun(st,Fun(st1,lt)),rv) =>{
        if(st==st1) reduce(FApp(Fun(st,lt),rv))
        else{
          Fun(st1,reduce(FApp(Fun(st,lt),rv)))
        }
      }  
      case FApp(FApp(lt,rt),rhs)=>{
        reduce(FApp(reduce(FApp(lt,rt)),rhs))
      }
      case FApp(FApp(lt,rt),FApp(lt1,rt1))=>{
        reduce(FApp(reduce(FApp(lt,rt)),reduce(FApp(lt1,rt1))))
      }
      case _ => x
      
    } 
  }

  def rename_process(t:Term): Term={
    t match {
        case FApp(Fun(st,Fun(st1,lt)),rv) =>{
            if(!rv.toString().contains(st1)) {
              t
            }
            else {
              FApp(Fun(st,rename_process(rename(st1,fresh.new_name(st1),Fun(st1,lt)))),rv)
            }
        }
        case _ =>{
          t
        }
        
    }
  }
  def change_let_in(t:Term): Term={
    t match {
        case Let(st,lt,rt) =>{
          rename_process(FApp(Fun(st,rt),lt) )
        }
        case Var(n) => t
        case Fun(arg,body) =>Fun(arg,change_let_in(body))
        case FApp(t1,t2) => FApp(change_let_in(t1),change_let_in(t2))
    }
  }
  
  
  object fresh {
    var cnt = 0
    def new_name(v:String):String = {
      cnt = cnt+1
      val new_n = "_"+v+"_"+(cnt.toString)
      new_n
    }
  }
  
  def rename(v:String,vn:String,t:Term): Term = {
    t match {
      case Var(n) => 
        if (v==n) Var(vn)
        else t
      case Fun(arg,body) => {
        if (v!=arg) t // no change
        else Fun(vn,rename(v,vn,body))
      }
      case FApp(t1,t2) => 
        FApp(rename(v,vn,t1),rename(v,vn,t2))
      case Let(w,t1,t2) => {
        val n_t1 = rename(v,vn,t1)
        if (v==w) Let(w,n_t1,t2)
        else Let(w,n_t1,rename(v,vn,t2))
      }
    }
}
  def test_eval(s:String) {
    try {
      val term = parse_lambda(s)
      println("Input      :"+s)
      println("Parsed Term:"+term)
      term match {
        case Some(t) => {
          val term1 = eval_to_value(t)
          println("Eval Value :"+term1)
//          val term2 = rename_process(t)
//          println("after rename process:"+term2)
        }
        case None => ()
      }
    } catch {
      case parseErr(s) => println("Parser error:"+s)
      case lexErr(s) => println("Lexer error:"+s)
    }
  }
  
  val ex_1 = """ xx_5  """
  val ex_2 = """ _xx_5  """
  val ex_3 = """ AB_5  CCC  """
  val ex_3a = """ AB_5  (X CCC)  """
  val ex_3b = """ AB_5 (X CCC) X """
  val ex_4 = """\ x . x x"""
  val ex_5 = """(\ x . x) x"""
  val ex_6 = """(\ x . x y)  x"""
  val ex_7 = """let y=hello in hello y"""
  val ex_8 = """let y=\x. x in y y"""
  val bad_1 = """($ x . x) x"""
  val bad_2 = """( x . 1) x"""
  val bad_3 = """5_hello"""
  val bad_4 = """\ . hello"""
  val bad_5 = """\ x. (hello"""
  val bad_6 = """\ x. ello)"""
  
  val ex_9a = """(\ x . (\y. y x) x) (\z. z)"""
  val ex_9b = """let id = \x.x in id id"""
  val ex_9c = """(\y . (\ x . x y)) (x x)"""
  val ex_9d = """(\y . (\ x . x y) z)"""
  val ex_9e = """(let id = \x.x in id id) y"""

  
  
  

  def test_lexer(s:String) {
    println("lexer:"+s+";")
    try {
      val toks = lex(s)
      println("Tokens read:"+toks)
    } catch {
      case (lexErr(s)) => println("Lexer error:"+s)
    }
  }

  def test_parser(s:String) {
    println("Before parse: "+s)
    try {
      
      val term = parse_lambda(s)
      println("Parsed Term:"+term)
    } catch {
      case parseErr(s) => println("Parser error:"+s)
      case lexErr(s) => println("Lexer error:"+s)
    }
  }

  test_lexer(ex_1)
  test_lexer(ex_2)
  test_lexer(ex_3)
  test_lexer(ex_4)
  test_lexer(ex_7)
  test_lexer(bad_1)
  test_lexer(bad_2)
 
  test_parser(ex_1)
  test_parser(ex_2)
  test_parser(ex_3)
  test_parser(ex_3a)
  test_parser(ex_3b)
  test_parser(ex_4)
  test_parser(ex_5)
  test_parser(ex_6)
  test_parser(ex_7)
  test_parser(ex_8)
  
  test_parser(bad_1)
  test_parser(bad_2)
  test_parser(bad_3)
  test_parser(bad_4)
  test_parser(bad_5)
  test_parser(bad_6)
  
  test_eval(ex_5)
  test_eval(ex_6)
  test_eval(ex_7)
  test_eval(ex_8)
  
  test_eval(ex_9a)
  test_eval(ex_9b)
  test_eval(ex_9c)
  test_eval(ex_9d)
  test_eval(ex_9e)
 
}
/*
 *

 // for e1 e2 .. en
 parse_term    : Parser[Token,Term]

 // for \x.e | (..) | v | let v=t1 in t2
 parse_factor  : Parser[Token,Term]

 // for let v=t1 in t2
 parse_let  : Parser[Token,Term]

 // for \ v1 .. vn . e
 parse_fun  : Parser[Token,Term]

 // for (e1)
 parse_bracket  : Parser[Token,Term]

 // for id
 parse_var    : Parser[Token,Term]
 
 // for a specific keyword
 parse_sym(x:String) : Parser[Token,Unit]


  parse_lambda("""\x.x""") ==>
  Lexical Analysis
     ==> List(Sym"\",Id "x",Sym".",Id "x"])
  Parser
     ==>
     * Fun("x", Var("x"))

* parse_lambda("""\x.(x x)""") ==>
     * Fun("x", App(Var("x"),Var("x")))

*/
