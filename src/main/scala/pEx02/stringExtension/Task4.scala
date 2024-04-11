package pEx02.stringExtension

import pEx02.stringExtension.Exp.*
import util.*

import Exp.*

def parse(se: SExp): Ext = ???



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




def interp(expr: Exp): Value = ???


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

