package hEx01

def flatten[A](xss : List[List[A]]) : List[A] = {
  def flattenHelper(remainder: List[List[A]], xs: List[A]): List[A] = {
    remainder match
      case Nil => xs
      case head :: tail => flattenHelper(tail, xs ++ head)
  }
  flattenHelper(xss, List())
}
