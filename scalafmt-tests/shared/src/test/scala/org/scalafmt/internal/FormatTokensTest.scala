package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig

import scala.meta._

class FormatTokensTest extends munit.FunSuite {

  private val tq = "\"\"\""
  // the literal's lines are as wide as the interpolation's
  private val interpolation = "object O {\n  val x = s" + tq +
    "$a\n       |\n       |The entire $b project's classpath is loaded to the repl.\n       |\n       |${c(d)}" +
    tq + ".stripMargin\n}\n"
  private val literal = interpolation.replace("s" + tq, tq).replace("$a", "aa")
    .replace("$b", "bb").replace("${c(d)}", "c(d)zzz")

  // the same string as a pattern: no stripMargin, so the string alone
  private val pattern = "object O {\n  y match { case " +
    interpolation.substring(interpolation.indexOf("s" + tq))
      .stripSuffix(".stripMargin\n}\n") + " => }\n}\n"

  private val xml = "object O {\n  val x = <div>\n" +
    "    <p>The entire {a} project's classpath is loaded to the repl.</p>\n" +
    "    <p>{b}</p>\n  </div>\n}\n"

  private def widthOf(code: String)(select: Tree => Tree): Int = {
    val tree = code.parse[Source].get
    new FormatOps(tree, ScalafmtConfig.default).tokens.width(select(tree))
  }

  private def widthOfRhs(code: String): Int = widthOf(code)(_.collect {
    case t: Defn.Val => t.rhs
  }.head)

  test("width of a multiline literal with stripMargin")(
    assertEquals(widthOfRhs(literal), 76),
  )

  test("width of a multiline interpolation with stripMargin")(
    assertEquals(widthOfRhs(interpolation), 92),
  )

  test("width of a multiline xml literal")(assertEquals(widthOfRhs(xml), 79))

  test("width of a multiline pattern interpolation")(
    assertEquals(widthOf(pattern)(_.collect { case t: Case => t.pat }.head), 80),
  )

}
