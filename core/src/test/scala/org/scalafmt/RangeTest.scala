package org.scalafmt

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.LoggerOps
import org.scalatest.FunSuite

abstract class AbstractRangeTest extends FunSuite with DiffAssertions {
  def check(original: String, expected: String, range: Range): Unit = {
    test(LoggerOps.reveal(original)) {
      val obtained = Scalafmt
        .format(original, ScalafmtConfig.unitTest40, range = Seq(range))
        .get
      assertNoDiff(obtained, expected)
    }
  }
}

class RangeTest extends AbstractRangeTest {
  check(
    """object a {
      |val x = 1
      |val y = 2
      |val z = 3
      |}
      |object   a
      |""".stripMargin,
    """object a {
      |val x = 1
      |  val y = 2
      |val z = 3
      |}
      |object   a
      |""".stripMargin,
    Range(3, 3)
  )
  check( // no matching line
    """
      |
      |object  a {
      |val x = 1
      |val y = 2
      |val z = 3
      |}
      |""".stripMargin,
    """
      |
      |object  a {
      |val x = 1
      |val y = 2
      |val z = 3
      |}
      |""".stripMargin,
    Range(1000, 10000)
  )
  check(
    """object a {
      |function(a,
      |b,
      |x)
      |}""".stripMargin,
    """object a {
      |  function(a, b, x)
      |}
      |""".stripMargin,
    Range(3, 3)
  )
  check(
    """object a {
      |  function(a,
      |           bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,
      |           x)
      |}""".stripMargin,
    """object a {
      |  function(
      |      a,
      |      bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb,
      |      x)
      |}
      |""".stripMargin,
    Range(3, 3)
  )
  check(
    """/**
      |    * a
      |  */
      |object   a
      |""".stripMargin,
    """/**
      |  * a
      |  */
      |object   a
      |""".stripMargin,
    Range(2, 2)
  )

  check(
    """|object a {
       |  lst.map{a=>
       |    l >2
       |    foo   ()
       |  }
       |}
       |""".stripMargin,
    """|object a {
       |  lst.map { a =>
       |    l > 2
       |    foo   ()
       |  }
       |}
       |""".stripMargin,
    Range(2, 3)
  )
}
