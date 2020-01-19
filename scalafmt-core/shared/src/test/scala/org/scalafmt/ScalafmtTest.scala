package org.scalafmt
import org.scalafmt.config.ScalafmtConfig
import org.scalameta.logger
import org.scalatest

class ScalafmtTest extends scalatest.funsuite.AnyFunSuite {
  def check(
      original: String,
      expected: String,
      config: ScalafmtConfig = ScalafmtConfig.default
  ): Unit = {
    test(logger.revealWhitespace(original).take(30)) {
      val obtained = Scalafmt.format(original, config).get
      if (obtained != expected) logger.elem(obtained)
      assert(obtained == expected)
    }
  }
  check(
    """
      |object A    {  println   ("HELLO!"  )  }
      |
      |
      |// comment
      """.stripMargin,
    """|object A { println("HELLO!") }
       |// comment
       |""".stripMargin
  )
  check(
    """|object A {
       |  val x = 2
       |  val xx = 3
       |}
    """.stripMargin,
    """|object A {
       |  val x  = 2
       |  val xx = 3
       |}
       |""".stripMargin,
    ScalafmtConfig.defaultWithAlign
  )
  val newlinesAtTheEOFConfig = ScalafmtConfig.default.copy(newlines =
    ScalafmtConfig.default.newlines.copy(alwaysAtTheEndOfFile = false)
  )
  check(
    """object A {
      |  val foo = 3
      |}
      |
      |
      |""".stripMargin,
    """object A {
      |  val foo = 3
      |}""".stripMargin,
    newlinesAtTheEOFConfig
  )
  check(
    """|object A { function(aaaaaaaa, bbbbbbbbbb, ddddd(eeeeeeeeee, fffffff, gggggggg)) }
    """.stripMargin,
    """|object A {
       |  function(
       |    aaaaaaaa,
       |    bbbbbbbbbb,
       |    ddddd(eeeeeeeeee, fffffff, gggggggg)
       |  )
       |}
       |""".stripMargin,
    ScalafmtConfig.default40
  )

}
