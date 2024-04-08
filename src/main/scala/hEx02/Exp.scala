package hEx02

enum Exp:
  case True()
  case False()
  case Not(expr: Exp)
  case And(lhs: Exp, rhs: Exp)
  case Or(lhs: Exp, rhs: Exp)
  case If(cond: Exp, thenExpr: Exp, elseExpr: Exp)
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
