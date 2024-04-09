package hEx02

import Exp.*

def desugar(expr: Ext): Exp = expr match
  case Ext.True() => True()
  case Ext.False() => False()
  case Ext.If(cond, thenExpr, elseExpr) => If(desugar(cond), desugar(thenExpr), desugar(elseExpr))
  case Ext.Num(n) => Num(n)
  case Ext.Plus(lhs, rhs) => Plus(desugar(lhs), desugar(rhs))
  case Ext.Mult(lhs, rhs) => Mult(desugar(lhs), desugar(rhs))
  
  // added cases
  case Ext.And(lhs, rhs) => And(desugar(lhs), desugar(rhs))
  case Ext.Or(lhs, rhs) => Or(desugar(lhs), desugar(rhs))
  case Ext.Not(expr) => Not(desugar(expr))
  case Ext.Implication(lhs, rhs) => Or(Not(desugar(lhs)), desugar(rhs))
  case Ext.BiImplication(lhs, rhs) => Or(And(Not(desugar(lhs)), Not(desugar(rhs))), And(desugar(lhs), desugar(rhs)))
  
  case _ => sys.error("couldn't parse given expression")


