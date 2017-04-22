package org.scalafmt
class ScalafmtTest extends org.scalatest.FunSuite {
  test("hello world") {
    val original =
      """
        |object A    {  println   ("HELLO!"  )  }
        |
        |
        |// comment
      """.stripMargin
    val obtained = Scalafmt.format(original).get
    org.scalameta.logger.elem(obtained)
    val expected =
      """|object A { println("HELLO!") }
         |// comment
         |""".stripMargin
    assert(obtained == expected)
  }
}
