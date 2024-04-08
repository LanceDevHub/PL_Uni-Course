package hEx07.CaseIf

enum Exp:
  case Num(num: Int)
  case Bool(b: Boolean)
  case NumEq(lhs: Exp, rhs: Exp)
  case Plus(lhs: Exp, rhs: Exp)
  case Minus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case App(fun: Exp, args: List[Exp])
  case Fun(params: List[String], body: Exp)
  case Id(name: String)
  case Rec(name: String, bound: Exp, body: Exp)

  case Constr(constrName: String, args: List[Exp])
  case Match(matchee: Exp, cases: List[CaseIf])


case class ADTDef(name: String, cotrs: List[(String, List[String])])
case class CaseIf(constrName: String, cond: Exp, patVars: List[String], body: Exp)
