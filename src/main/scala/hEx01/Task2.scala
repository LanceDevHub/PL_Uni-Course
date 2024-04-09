package hEx01

def reverse[A](l: List[A]): List[A] = {
  def reverseHelper(list: List[A], accum: List[A]): List[A] = {
    list match
      case head :: tail => reverseHelper(tail, head :: accum)
      case Nil => accum
  }
  reverseHelper(l, List())
}

