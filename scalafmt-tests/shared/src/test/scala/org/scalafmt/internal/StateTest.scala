package org.scalafmt.internal

import org.scalafmt.config.ScalafmtConfig

import scala.meta._
import scala.meta.tokens.{Token => T}

class StateTest extends munit.FunSuite {

  private val xml = "object O {\n  val x = <div>\n" +
    "    <p>The entire {a} project's classpath is loaded to the repl.</p>\n" +
    "    <p>{b}</p>\n  </div>\n}\n"

  test("columns of a multiline xml part") {
    val tree = xml.parse[Source].get
    val tokens = new FormatOps(tree, ScalafmtConfig.default).tokens
    val ft = tokens.arr.find(x => x.left.is[T.Xml.Part] && x.meta.left.hasNL)
      .get
    val columns = State.getColumns(ft.left, ft.meta.left, 0)(identity)(identity)
    assertEquals(columns, (5, 18))
  }

}
