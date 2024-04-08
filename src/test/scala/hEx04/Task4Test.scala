package hEx04

import org.scalatest.funsuite.AnyFunSuite

class Task4Test extends AnyFunSuite:
  val text = List("word", "another", "word", "yet", "another", "word")


  test("word counting") {
    val map: String => List[(String, Int)] = word => List((word, 1))
    val reduce: (String, List[Int]) => (String, Int) = (word, counts) => (word, counts.size)
    assertResult(
      Map("word" -> 3, "another" -> 2, "yet" -> 1)
    )(
      mapReduce[String, String, String, Int, Int](text, map, reduce)
    )
  }

  test("letter counting") {
    val map = ??? // Implement it
    val reduce = ??? // Implement it
    assertResult(
      Map('e' -> 3, 'n' -> 2, 'y' -> 1, 't' -> 3, 'a' -> 2, 'h' -> 2, 'r' -> 5, 'w' -> 3, 'o' -> 5, 'd' -> 3)
    )(
      mapReduce(text, map, reduce)
    )
  }


  /* inputGraph:
  v1 ---10--> v2 <--3--- v5
              |          ^
              5          |
              |          2
              V          |
              v3 ---0--> v4

  */

  case class Edge(tail: Int, head: Int, weight: Int)    // an edge goes from tail to head
  val inputGraph = List(Edge(1, 2, 10), Edge(4, 5, 2), Edge(2, 3, 5), Edge(5, 2, 3), Edge(3, 4, 0))

  test("weight summation") {
    val map = ??? // Implement it
    val reduce = ??? // Implement it

    assertResult(
      Map(    // (node number, summed weights of incoming edges)
        (1, 0), (2, 13), (3, 5), (4, 0), (5, 2)
      )
    )(
      mapReduce(inputGraph, map, reduce)
    )
  }
