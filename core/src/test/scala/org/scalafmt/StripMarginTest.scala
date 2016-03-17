package org.scalafmt

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.HasTests
import org.scalatest.FunSuite

class StripMarginTest extends FunSuite with HasTests with DiffAssertions {

  val examples = """
<<< Align | margin 1
val x =
'''Formatter changed AST
          |=====================
          |$diff
        '''
        .stripMargin
>>>
val x = '''Formatter changed AST
          |=====================
          |$diff
        '''.stripMargin
<<< Align | margin 2
{
  val x = 1
  '''UNABLE TO FORMAT,
    |fooooo baaaaaaaarrrrr
    |'''.stripMargin
}
>>>
{
  val x = 1
  '''UNABLE TO FORMAT,
    |fooooo baaaaaaaarrrrr
    |'''.stripMargin
}
  """.replace("'''", "\"\"\"")

  val interpolateExamples = """
<<< break indentation
val msg =

  s'''AAAAAAA
      |${mkString(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbb)}
  |'''.stripMargin
>>>
val msg =
  s'''AAAAAAA
     |${mkString(
         aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbb)}
     |'''.stripMargin
<<< Align | margin 2
val msg =

  s'''UNABLE TO FORMAT,
  |${mkString(deepestYet.splits)}
  |'''.stripMargin
>>>
val msg = s'''UNABLE TO FORMAT,
             |${mkString(deepestYet.splits)}
             |'''.stripMargin
<<< don't break if no need
val msg =

  s'''AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  |${mkString(deepestYet.splits)}
  |'''.stripMargin
>>>
val msg = s'''AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
             |${mkString(deepestYet.splits)}
             |'''.stripMargin""".replace("'''", "\"\"\"")

  override val tests =
    parseDiffTests(examples, "stripMargin/String.stat") ++
    (parseDiffTests(interpolateExamples, "stripMargin/Interpolate.stat"))

  tests.foreach { t =>
    test(t.fullName) {
      val parse = filename2parse(t.filename).get
      val formatted =
        ScalaFmt.format_!(t.original, ScalaStyle.StripMarginTest)(parse)
      assertNoDiff(formatted, t.expected)
    }
  }
}
