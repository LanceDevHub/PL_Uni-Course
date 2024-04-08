package hEx07.PatternMatching

enum Exp:
  case Num(num: Int)
  case Plus(lhs: Exp, rhs: Exp)
  case Mult(lhs: Exp, rhs: Exp)
  case App(fun: Exp, args: List[Exp])
  case Fun(params: List[String], body: Exp)
  case Id(name: String)
  case Rec(name: String, bound: Exp, body: Exp)

  case Constr(constrName: String, args: List[Exp])
  case Match(matchee: Exp, cases: List[Case])


case class ADTDef(name: String, cotrs: List[(String, List[String])])
case class Case(constrName: String, patVars: List[String], body: Exp)
