package pEx02.stringExtension

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

  // strings and string operations
  case Str(str: String)
  case StrEquals(str1: Exp, str2: Exp)
  case CharAt(str: Exp, index: Exp)
  case Concat(str1: Exp, str2: Exp)
  case Length(str: Exp)
