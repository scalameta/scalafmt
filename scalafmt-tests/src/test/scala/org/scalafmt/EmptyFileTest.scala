package org.scalafmt

import scala.meta.internal.prettyprinters.DoubleQuotes

import java.lang.System.lineSeparator

import munit.FunSuite

class EmptyFileTest extends FunSuite {

  Seq(
    ("", "\n"),
    ("  \n  \n  ", "\n"),
    ("  \r\n  \r\n  ", "\n"),
    (lineSeparator(), "\n"),
    (s"   $lineSeparator  ", "\n"),
  ).foreach { case (original, expected) =>
    val expectedQuoted = DoubleQuotes(expected)
    test(s"empty tree formats to newline: ${DoubleQuotes(original)} -> $expectedQuoted") {
      val obtained = Scalafmt.format(original).get
      if (obtained != expected) fail(
        s"values are not equal: ${DoubleQuotes(obtained)} != $expectedQuoted",
      )
    }
  }

}
