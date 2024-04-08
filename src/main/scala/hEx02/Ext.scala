package hEx02


enum Ext:
  case True()
  case False()
  case If(cond: Ext, thenExpr: Ext, elseExpr: Ext)
  case Num(num: Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)

  // TODO add ast nodes
