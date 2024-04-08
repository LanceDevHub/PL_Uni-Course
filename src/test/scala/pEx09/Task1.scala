package pEx09

import org.scalatest.funsuite.AnyFunSuite

class Task1 extends AnyFunSuite:

  /*
   * Let's define and invoke a method in the super class Bool
   */
  val boolClasses =
    """
      |  (class Bool Object
      |    (method or b
      |      (((this not) and (b not)) not))
      |  )
      |  (class True Bool
      |    (method and b
      |      b)
      |    (method not
      |      (new False))
      |  )
      |  (class False Bool
      |    (method and b
      |      this)
      |    (method not
      |      (new True))
      |  )
    """.stripMargin

    // TODO: Insert your test cases here. You can use the boolean classes from above to write meaningful test cases

