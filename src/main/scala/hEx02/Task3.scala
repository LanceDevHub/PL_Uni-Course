package hEx02

import Exp.*

import scala.annotation.tailrec

enum Nat:
  case Zero()
  case Succ(n: Nat)
import Nat.*

def add(n: Nat, m: Nat): Nat = n match
  case Zero() => m
  case Succ(n1) => add(n1, Succ(m))

def mult(n: Nat, m: Nat): Nat = n match
  case Zero() => Zero()
  case Succ(n1) => add(mult(m, n1), m)

// The precondition n >= 0 has to hold

def intToPeano(n: Int): Nat = {
  def intToPeanoHelper(num: Int, result: Nat = Zero()): Nat = {
    num match
      case 0 => result
      case n: Int if (n >= 1) => intToPeanoHelper(n - 1, Succ(result))
      case _ => sys.error("given number can't be converted to peano")
  }

  intToPeanoHelper(n)
}

// easier way intToPeano -> n match case 0 => Zero() case _ => Succ(intToPeano(n-1))

enum Value:
  case Num(n: Nat)
  case Bool(b: Boolean)

def interp(expr: Exp): Value = expr match
  case True() => Value.Bool(true)
  
  case False() => Value.Bool(false)
  
  case Not(expr) => interp(expr) match
    case Value.Bool(true) => Value.Bool(false)
    case Value.Bool(false) => Value.Bool(true)
    case e => sys.error(s"couldn't interpret given expression because of $e")
    
  case And(lhs, rhs) => interp(lhs) match
    case Value.Bool(true) => interp(rhs) match 
      case Value.Bool(true) => Value.Bool(true)
      case Value.Bool(false) => Value.Bool(false)
      case e => sys.error(s"couldn't interpret given expression because of $e")
    case Value.Bool(false) => Value.Bool(false)
    case e => sys.error(s"couldn't interpret given expression because of $e")
    
  case Or(lhs, rhs) => interp(lhs) match
    case Value.Bool(true) => Value.Bool(true)
    case Value.Bool(false) => interp(rhs) match
      case Value.Bool(true) => Value.Bool(true)
      case Value.Bool(false) => Value.Bool(false)
      case e => sys.error(s"couldn't interpret given expression because of $e")
    case e => sys.error(s"couldn't interpret given expression because of $e")
    
  case If(cond, thenExpr, elseExpr) => interp(cond) match
    case Value.Bool(true) => interp(thenExpr) match
      case expr: Value => expr
      case e => sys.error(s"couldn't interpret given expression because of $e")
    case Value.Bool(false) => interp(elseExpr) match
      case expr: Value => expr
      case e => sys.error(s"couldn't interpret given expression because of $e")
    case e => sys.error(s"couldn't interpret given expression because of $e") 
    
  case Num(n: Int) => Value.Num(intToPeano(n))
  
  case Plus(lhs, rhs) => interp(lhs) match
    case Value.Num(nat1) => interp(rhs) match
      case Value.Num(nat2) => Value.Num(add(nat1, nat2))
      case e => sys.error(s"couldn't interpret given expression because of $e")
    case e => sys.error(s"couldn't interpret given expression because of $e")
    
  case Mult(lhs, rhs) => interp(lhs) match
    case Value.Num(nat1) => interp(rhs) match
      case Value.Num(nat2) => Value.Num(mult(nat1, nat2))
      case e => sys.error(s"couldn't interpret given expression because of $e")
    case e => sys.error(s"couldn't interpret given expression because of $e")

  case e => sys.error(s"couldn't interpret given expression because of $e")
      
  
  
  
