package hEx01

def reverse[A](l: List[A]): List[A] = {
  def reverseHelper(list: List[A], accum: List[A]): List[A] = {
    list match
      case Nil => accum
      case head :: tail => reverseHelper(tail, head :: accum)
  }
  reverseHelper(l, List())
}

