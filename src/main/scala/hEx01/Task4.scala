package hEx01

import hEx02.Exp.True

def merge[A, B](m1: Map[A, B], m2: Map[A, B]): Map[A, B] = {
  var resultMap = m1
  val keys = m2.keys
  keys.foreach(key => if(m1.contains(key)) sys.error("redundant keys occurred"))
  keys.foreach(key => resultMap = resultMap + (key -> m2.apply(key)))
  return resultMap
}
