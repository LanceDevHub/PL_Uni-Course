package hEx02

import util.*


def parse(se: SExp): Ext = se match
  case SExp.Sym("true") => Ext.True()
  case SExp.Sym("false") => Ext.False()
  case SExp.List(List(SExp.Sym("if"), cond, thenExpr, elseExpr)) => Ext.If(parse(cond), parse(thenExpr), parse(elseExpr))
  case SExp.Num(n) => Ext.Num(n)
  case SExp.List(List(SExp.Sym("+"), lhs, rhs)) => Ext.Plus(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("*"), lhs, rhs)) => Ext.Mult(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("and"), lhs, rhs)) => Ext.And(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("or"), lhs, rhs)) => Ext.Or(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("not"), expr)) => Ext.Not(parse(expr))
  case SExp.List(List(SExp.Sym("=>"), lhs, rhs)) => Ext.Implication(parse(lhs), parse(rhs))
  case SExp.List(List(SExp.Sym("<=>"), lhs, rhs)) => Ext.BiImplication(parse(lhs), parse(rhs))
  case _ => sys.error("couldn't parse given expression")
  

