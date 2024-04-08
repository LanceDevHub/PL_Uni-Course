package pEx03

import Exp.*

def countBindingOccurrencesOfName(exp: Exp, name: String): Int =
  val rec = countBindingOccurrencesOfName(_, name)
  exp match
    case Num(n) => 0
    case Plus(e1, e2) => rec(e1) + rec(e2)
    case Let(nameOfLet, bound, body) =>
      val countOfChildren = rec(bound) + rec(body)
      if (name == nameOfLet)
        1 + countOfChildren
      else
        0 + countOfChildren
    case Id(name) => 0

def rename(exp: Exp, oldName: String, newName: String): Exp =
  def renameWithoutCheckingForMultipleBindings(exp: Exp, oldName: String, newName: String): Exp =
    val rec = renameWithoutCheckingForMultipleBindings(_, oldName, newName)
    exp match
      case Num(n) => exp
      case Plus(e1, e2) => Plus(rec(e1), rec(e2))
      case Id(name) => exp
      case Let(name, bound, body) => ???

  countBindingOccurrencesOfName(exp, oldName) match
    case 0 => exp
    case 1 => renameWithoutCheckingForMultipleBindings(exp, oldName, newName)
    case _ => sys.error(s"There are multiple binding occurrences of $oldName")

