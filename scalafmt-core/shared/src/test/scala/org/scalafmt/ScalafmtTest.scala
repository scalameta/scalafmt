package org.scalafmt

import munit.FunSuite
import org.scalafmt.config.ScalafmtConfig
import org.scalameta.logger

class ScalafmtTest extends FunSuite {
  def check(
      original: String,
      expected: String,
      config: ScalafmtConfig = ScalafmtConfig.default
  )(implicit loc: munit.Location): Unit = {
    test(logger.revealWhitespace(original).take(30)) {
      val obtained = Scalafmt.formatCode(original, config).get
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
    config.ScalafmtConfig.defaultWithAlign
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
    config.ScalafmtConfig.default40
  )

}
