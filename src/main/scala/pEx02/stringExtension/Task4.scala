package pEx02.stringExtension

import pEx02.stringExtension.Exp.*
import util.*

import Exp.*

def parse(se: SExp): Ext = se match
  case SExp.Sym("true") => Ext.True()
  case SExp.Sym("false") => Ext.False()
  case SExp.List(List(SExp.Sym("and"), lhs, rhs)) => Ext.And(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("or"), lhs, rhs)) => Ext.Or(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("not"), expr)) => Ext.Not(parse(expr))
  case SExp.List(List(SExp.Sym("if"), cond, thenExpr, elseExpr)) =>
    Ext.If(parse(cond), parse(thenExpr), parse(elseExpr))
  case SExp.List(List(SExp.Sym("=>"), lhs, rhs)) => Ext.Implication(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("<=>"), lhs, rhs)) => Ext.BiImplication(parse(lhs), parse(rhs))
  case SExp.Num(n) => Ext.Num(n)
  case SExp.List(List(SExp.Sym("+"), lhs, rhs)) => Ext.Plus(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("*"), lhs, rhs)) => Ext.Mult(parse(lhs), parse(rhs))
  // strings and string operations
  case SExp.List(List(SExp.Sym("str"), SExp.Sym(stringName))) => Ext.Str(stringName)
  case SExp.List(List(SExp.Sym("strEquals"), str1, str2)) => Ext.StrEquals(parse(str1), parse(str2))
  case SExp.List(List(SExp.Sym("charAt"), str, index)) => Ext.CharAt(parse(str), parse(index))
  case SExp.List(List(SExp.Sym("head"), str)) => Ext.Head(parse(str))
  case SExp.List(List(SExp.Sym("startsWith"), str, firstLetter)) =>
    Ext.StartsWith(parse(str), parse(firstLetter))
  case SExp.List(List(SExp.Sym("endsWith"), str, lastLetter)) =>
    Ext.EndsWith(parse(str), parse(lastLetter))
  case SExp.List(List(SExp.Sym("concat"), str1, str2)) =>
    Ext.Concat(parse(str1), parse(str2))
  case SExp.List(List(SExp.Sym("length"), str)) => Ext.Length(parse(str))

  case e => sys.error(s"couldn't parse given expression because of $e")


def desugar(expr: Ext): Exp = expr match
  case Ext.True() => True()
  case Ext.False() => False()
  case Ext.Not(expr) => Not(desugar(expr))
  case Ext.And(lhs, rhs) => And(desugar(lhs), desugar(rhs))
  case Ext.Or(lhs, rhs) => Or(desugar(lhs), desugar(rhs))
  case Ext.If(cond, thenExpr, elseExpr) => If(desugar(cond), desugar(thenExpr), desugar(elseExpr))
  case Ext.Implication(lhs, rhs) => Or(Not(desugar(lhs)), desugar(rhs))
  case Ext.BiImplication(lhs, rhs) => Or(And(Not(desugar(lhs)), Not(desugar(rhs))), And(desugar(lhs), desugar(rhs)))
  case Ext.Num(n) => Num(n)
  case Ext.Plus(lhs, rhs) => Plus(desugar(lhs), desugar(rhs))
  case Ext.Mult(lhs, rhs) => Mult(desugar(lhs), desugar(rhs))

  // string operations
  case Ext.Str(s) => Str(s)
  case Ext.StrEquals(str1, str2) => StrEquals(desugar(str1), desugar(str2))
  case Ext.CharAt(str, index) => CharAt(desugar(str), desugar(index))
  case Ext.Head(str) => CharAt(desugar(str), Num(1))
  case Ext.StartsWith(str, firstLetter) => StrEquals(desugar(Ext.Head(str)), desugar(firstLetter))
  case Ext.EndsWith(str, lastLetter) =>
    val strExpr = desugar(str)
    StrEquals(CharAt(strExpr, Length(strExpr)), desugar(lastLetter))
  case Ext.Concat(str1, str2) => Concat(desugar(str1), desugar(str2))
  case Ext.Length(str) => Length(desugar(str))



enum Nat:
  case Zero()
  case Succ(n: Nat)

enum Value:
  case Num(n: Nat)
  case Bool(b: Boolean)
  case Str(s: String)

  def int: Int = this match
    case Num(n: Nat) => peanoToInt(n)
    case e => sys.error(s"cant convert $e(value) to int.")

  def str: String = this match
    case Str(str: String) => str
    case e => sys.error(s"cant convert $e(value) to string.")




def interp(expr: Exp): Value = expr match
  case True() => Value.Bool(true)
  case False() => Value.Bool(false)

  case Not(expr) => interp(expr) match
    case Value.Bool(true) => Value.Bool(false)
    case Value.Bool(false) => Value.Bool(true)
    case e => sys.error(s"couldn't parse $e. Invalid input $e!")

  case And(lhs, rhs) => interp(lhs) match
    case Value.Bool(true) => interp(rhs) match
      case Value.Bool(true) => Value.Bool(true)
      case Value.Bool(false) => Value.Bool(false)
      case e => sys.error(s"couldn't parse $e. Invalid input $e!")
    case Value.Bool(false) => Value.Bool(false)
    case e => sys.error(s"couldn't parse $e. Invalid input $e!")

  case Or(lhs, rhs) => interp(lhs) match
    case Value.Bool(true) => Value.Bool(true)
    case Value.Bool(false) => interp(rhs) match
      case Value.Bool(true) => Value.Bool(true)
      case Value.Bool(false) => Value.Bool(false)
      case e => sys.error(s"couldn't parse $e. Invalid input $e!")
    case e => sys.error(s"couldn't parse $e. Invalid input $e!")

  case If(cond, thenExpr, elseExpr) => interp(cond) match
    case Value.Bool(true) => interp(thenExpr)
    case Value.Bool(false) => interp(elseExpr)
    case e => sys.error(s"couldn't parse $e. Invalid input $e!")

  case Num(n) => Value.Num(intToPeano(n))

  case Plus(lhs, rhs) => interp(lhs) match
    case Value.Num(nat1) => interp(rhs) match
      case Value.Num(nat2) => Value.Num(add(nat1, nat2))
      case e => sys.error(s"couldn't parse $e. Invalid input $e!")
    case e => sys.error(s"couldn't parse $e. Invalid input $e!")

  case Mult(lhs, rhs) => interp(lhs) match
    case Value.Num(nat1) => interp(rhs) match
      case Value.Num(nat2) => Value.Num(mult(nat1, nat2))
      case e => sys.error(s"couldn't parse $e. Invalid input $e!")
    case e => sys.error(s"couldn't parse $e. Invalid input $e!")

  // strings and string operations
  case Str(str) => Value.Str(str)
  case StrEquals(str1, str2) => Value.Bool(interp(str1).str == interp(str2).str)
  case CharAt(str, index) => Value.Str(interp(str).str.charAt(interp(index).int - 1).toString)
  case Concat(str1, str2) => Value.Str(interp(str1).str.concat(interp(str2).str))
  case Length(str) => Value.Num(intToPeano(interp(str).str.length))
  
  case e => sys.error(s"couldn't parse $e. Invalid input $e!")

import Nat.*
def add(n: Nat, m: Nat): Nat = n match
  case Zero() => m
  case Succ(n1) => add(n1, Succ(m))

def mult(n: Nat, m: Nat): Nat = n match
  case Zero() => Zero()
  case Succ(n1) => add(mult(m, n1), m)

// The pre condition n >= 0 has to hold

def intToPeano(n: Int): Nat = n match
  case 0 => Zero()
  case _ => Succ(intToPeano(n-1))


def peanoToInt(nat: Nat): Int = nat match
  case Zero() => 0
  case Succ(x) => 1 + peanoToInt(x)

