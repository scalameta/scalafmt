package org.scalafmt
import org.scalafmt.config.ScalafmtConfig
import org.scalameta.logger

class ScalafmtTest extends org.scalatest.FunSuite {
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
  check(
    """|object Foo {
       |  function(
       |    a &&
       |    b
       |  )
       |}
       |""".stripMargin,
    """|object Foo {
       |  function(
       |    a &&
       |    b
       |  )
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(maxColumn = 10, unindentTopLevelOperators = true)
  )
  check(
    """|object Foo1 {
       |  function(
       |    a &&
       |    b
       |  )
       |}
       |""".stripMargin,
    """|object Foo1 {
       |  function(
       |    a &&
       |    b
       |  )
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(maxColumn = 10)
  )

  check(
    """|object Foo {
       |  val x =
       |    a +
       |    b
       |}
       |""".stripMargin,
    """|object Foo {
       |  val x =
       |    a +
       |    b
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(maxColumn = 10, unindentTopLevelOperators = true)
  )
  check(
    """|object Foo2 {
       |  val x =
       |    a +
       |    b
       |}
       |""".stripMargin,
    """|object Foo2 {
       |  val x =
       |    a +
       |    b
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(maxColumn = 10)
  )

  check(
    """|object Foo {
       |  a +
       |  b
       |}
       |""".stripMargin,
    """|object Foo {
       |  a +
       |  b
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(maxColumn = 6, unindentTopLevelOperators = true)
  )

  check(
    """|object Foo3 {
       |  a +
       |  b
       |}
       |""".stripMargin,
    """|object Foo3 {
       |  a +
       |  b
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(maxColumn = 6)
  )

}
