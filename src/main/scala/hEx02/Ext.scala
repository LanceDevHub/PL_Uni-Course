package hEx02


enum Ext:
  case True()
  case False()
  case If(cond: Ext, thenExpr: Ext, elseExpr: Ext)
  case Num(num: Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  // added cases
  case And(lhs: Ext, rhs: Ext)
  case Or(lhs: Ext, rhs: Ext)
  case Not(expr: Ext)
  case Implication(lhs: Ext, rhs: Ext)
  case BiImplication(lhs: Ext, rhs: Ext)

