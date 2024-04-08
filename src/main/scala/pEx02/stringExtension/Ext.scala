package pEx02.stringExtension

enum Ext:
  case True()
  case False()
  case And(lhs: Ext, rhs: Ext)
  case Or(lhs: Ext, rhs: Ext)
  case Not(expr: Ext)
  case If(cond: Ext, thenExpr: Ext, elseExpr: Ext)

  case BiImplication(lhs: Ext, rhs: Ext)
  case Implication(lhs: Ext, rhs: Ext)

  case Num(num: Int)
  case Plus(lhs: Ext, rhs: Ext)
  case Mult(lhs: Ext, rhs: Ext)
  
  // strings and string operations
  case Str(str: String)
  case StrEquals(str1: Ext, str2: Ext)
  case CharAt(str: Ext, index: Ext)
  case Head(str: Ext)
  case StartsWith(str: Ext, firstLetter: Ext)
  case EndsWith(str: Ext, lastLetter: Ext)
  case Concat(str1: Ext, str2: Ext)
  case Length(str: Ext)
