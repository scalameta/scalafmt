package org.scalafmt
import org.scalafmt.config.{DanglingParentheses, ScalafmtConfig}
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
    config.ScalafmtConfig.default40
      .copy(maxColumn = 10, verticalAlignMultilineOperators = true)
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
    config.ScalafmtConfig.default40
      .copy(maxColumn = 10, verticalAlignMultilineOperators = true)
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
    config.ScalafmtConfig.default40
      .copy(maxColumn = 6, verticalAlignMultilineOperators = true)
  )

  check(
    """|object Foo3 {
       |  def f(a: A, b: B, c: C) = 1
       |  class F(a: A, b: B, c: C)
       |
       |  f(a, b, c)
       |  new F(a, b, c)
       |}
       |""".stripMargin,
    """|object Foo3 {
       |  def f(
       |      a: A,
       |      b: B,
       |      c: C) =
       |    1
       |  class F(
       |      a: A,
       |      b: B,
       |      c: C)
       |
       |  f(
       |    a,
       |    b,
       |    c
       |  )
       |  new F(
       |    a,
       |    b,
       |    c
       |  )
       |}
       |""".stripMargin,
    config.ScalafmtConfig.default40.copy(
      maxColumn = 6,
      danglingParentheses = DanglingParentheses(true, false)
    )
  )

}
