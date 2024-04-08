package pEx02.stringExtension

import pEx02.stringExtension.Exp.*
import util.*

import Exp.*

def parse(se: SExp): Ext = se match
  case SExp.List(List(SExp.Sym("str"), SExp.Sym(str))) => Ext.Str(str)
  case SExp.List(List(SExp.Sym("strEquals"), str1, str2)) => Ext.StrEquals(parse(str1), parse(str2))
  case SExp.List(List(SExp.Sym("charAt"), str, index)) => Ext.CharAt(parse(str), parse(index))
  case SExp.List(List(SExp.Sym("head"), str)) => Ext.Head(parse(str))
  case SExp.List(List(SExp.Sym("startsWith"), str, firstLetter)) => Ext.StartsWith(parse(str), parse(firstLetter))
  case SExp.List(List(SExp.Sym("endsWith"), str, lastLetter)) => Ext.EndsWith(parse(str), parse(lastLetter))
  case SExp.List(List(SExp.Sym("concat"), str1, str2)) => Ext.Concat(parse(str1), parse(str2))
  case SExp.List(List(SExp.Sym("length"), str)) => Ext.Length(parse(str))

  case SExp.Num(n) => Ext.Num(n)
  case SExp.Sym("true") => Ext.True()
  case SExp.Sym("false") => Ext.False()
  case SExp.List(List(SExp.Sym("and"), lhs, rhs)) => Ext.And(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("or"), lhs, rhs)) => Ext.Or(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("not"), expr)) => Ext.Not(parse(expr))
  case SExp.List(List(SExp.Sym("<=>"), lhs, rhs)) => Ext.BiImplication(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("=>"), lhs, rhs)) => Ext.Implication(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("if"), cond, thenBranch, elseBranch)) =>
    Ext.If(parse(cond), parse(thenBranch), parse(elseBranch))
  case SExp.List(List(SExp.Sym("+"), lhs, rhs)) => Ext.Plus(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("*"), lhs, rhs)) => Ext.Mult(parse(lhs), parse(rhs))

  case _ => sys.error(s"$se could not be parsed")


def desugar(expr: Ext): Exp = expr match
  case Ext.Str(str) => Exp.Str(str)
  case Ext.StrEquals(str1, str2) => Exp.StrEquals(desugar(str1), desugar(str2))
  case Ext.CharAt(str, index) => Exp.CharAt(desugar(str), desugar(index))
  case Ext.Head(str) => Exp.CharAt(desugar(str), Exp.Num(1))
  case Ext.StartsWith(str, firstLetter) => Exp.StrEquals(desugar(Ext.Head(str)), desugar(firstLetter))
  case Ext.EndsWith(str, lastLetter) =>
    val strExp = desugar(str)
    Exp.StrEquals(
      Exp.CharAt(strExp, Exp.Length(strExp)),
      desugar(lastLetter))
  case Ext.Concat(str1, str2) => Exp.Concat(desugar(str1), desugar(str2))
  case Ext.Length(str) => Exp.Length(desugar(str))

  case Ext.True() => True()
  case Ext.False() => False()
  case Ext.Not(e) => Not(desugar(e))
  case Ext.And(lhs, rhs) => And(desugar(lhs), desugar(rhs))
  case Ext.Or(lhs, rhs) => Or(desugar(lhs), desugar(rhs))
  case Ext.If(cond, thenExpr, elseExpr) =>
    If(desugar(cond), desugar(thenExpr), desugar(elseExpr))
  case Ext.Num(n) => Num(n)
  case Ext.Plus(lhs, rhs) => Plus(desugar(lhs), desugar(rhs))
  case Ext.Mult(lhs, rhs) => Mult(desugar(lhs), desugar(rhs))
  case Ext.Implication(lhs, rhs) => Or(Not(desugar(lhs)), desugar(rhs))
  case Ext.BiImplication(lhs, rhs) => Or(And(Not(desugar(lhs)), Not(desugar(rhs))), And(desugar(lhs), desugar(rhs)))

enum Nat:
  case Zero()
  case Succ(n: Nat)

enum Value:
  case Num(n: Nat)
  case Bool(b: Boolean)
  case Str(s: String)

  def str: String = this match
    case Str(s) => s
    case _ => sys.error(s"$this should be a Value.String, but isn't")

  def int: Int = this match
    case Num(nat) => peanoToInt(nat)
    case _ => sys.error(s"$this should be a Value.String, but isn't")

def interp(expr: Exp): Value = expr match
  case Str(str) => Value.Str(str)
  case StrEquals(str1, str2) => Value.Bool(interp(str1).str == interp(str2).str)
  case CharAt(str, index) => Value.Str(interp(str).str
    .charAt(interp(index).int - 1).toString)
  case Concat(str1, str2) => Value.Str(interp(str1).str.concat(interp(str2).str))
  case Length(str) => Value.Num(intToPeano(interp(str).str.length))


  case Num(n) => Value.Num(intToPeano(n))
  case True() => Value.Bool(true)
  case False() => Value.Bool(false)
  case Not(e) => interp(e) match {
    case Value.Bool(true) => Value.Bool(false)
    case Value.Bool(false) => Value.Bool(true)
    case ev => sys.error("not expected a boolean value, but got " + ev)
  }
  case And(lhs, rhs) => interp(lhs) match {
    case Value.Bool(false) => Value.Bool(false)
    case Value.Bool(lhv) => interp(rhs) match {
      case Value.Bool(true) => Value.Bool(true)
      case Value.Bool(false) => Value.Bool(false)
      case rhv => sys.error("and expected a boolean value, but got " + rhv)
    }
    case lhv => sys.error("and expected a boolean value, but got " + lhv)
  }
  case Or(lhs, rhs) => interp(lhs) match {
    case Value.Bool(true) => Value.Bool(true)
    case Value.Bool(lhv) => interp(rhs) match {
      case Value.Bool(true) => Value.Bool(true)
      case Value.Bool(false) => Value.Bool(false)
      case rhv => sys.error("and expected a boolean value, but got " + rhv)
    }
    case lhv => sys.error("and expected a boolean value, but got " + lhv)
  }
  case If(cond, thenExpr, elseExpr) => interp(cond) match {
    case Value.Bool(true) => interp(thenExpr)
    case Value.Bool(false) => interp(elseExpr)
    case condv => sys.error("if expected a boolean value for cond, but got " + condv)
  }
  case Plus(lhs, rhs) => interp(lhs) match {
    case Value.Num(lhv) => interp(rhs) match {
      case Value.Num(rhv) => Value.Num(add(lhv, rhv))
      case rhv => sys.error("+ expected a number value, but got " + rhv)
    }
    case lhv => sys.error("+ expected a number value, but got " + lhv)
  }
  case Mult(lhs, rhs) => interp(lhs) match {
    case Value.Num(lhv) => interp(rhs) match {
      case Value.Num(rhv) => Value.Num(mult(lhv, rhv))
      case rhv => sys.error("* expected a number value, but got " + rhv)
    }
    case lhv => sys.error("* expected a number value, but got " + lhv)
  }


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
  case _ => Succ(intToPeano(n - 1))

def peanoToInt(nat: Nat): Int = nat match {
  case Nat.Zero() => 0
  case Nat.Succ(n) => 1 + peanoToInt(n)
}

