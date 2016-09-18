package org.scalafmt

import scala.meta.Tree
import scala.meta.parsers.Parse

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.DiffTest
import org.scalafmt.util.HasTests
import org.scalatest.FunSuite

class StripMarginTests extends FunSuite with HasTests with DiffAssertions {

  val rawStrings =
    """
<<< Align | margin 1
val x = '''Formatter changed AST
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
<<< Align | margin 3
val x = '''Short line
          |Long line aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
          |'''.stripMargin
>>>
val x =
  '''Short line
    |Long line aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    |'''.stripMargin
<<< pipe char on first line #324
'''|first
  |second'''.stripMargin
>>>
'''|first
   |second'''.stripMargin
  """.replace("'''", "\"\"\"")

  val interpolatedStrings =
    """
<<< tricky tempalate curly
case class FormatterChangedAST(diff: String, output: String)
    extends Error(s'''Formatter changed AST
        |=====================
        |$diff
        |=====================
        |${output.lines.toVector.mkString("\n")}
        |=====================
        |Formatter changed AST
    |'''.stripMargin)
>>>
case class FormatterChangedAST(diff: String, output: String)
    extends Error(s'''Formatter changed AST
                     |=====================
                     |$diff
                     |=====================
                     |${output.lines.toVector.mkString("\n")}
                     |=====================
                     |Formatter changed AST
                     |'''.stripMargin)
<<< break indentation
val msg =

  s'''AAAAAAA
      |${mkString(aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, bbb)}
  |'''.stripMargin
>>>
val msg =
  s'''AAAAAAA
     |${mkString(
       aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa,
       bbb)}
     |'''.stripMargin
<<< Align | margin 2
val msg = s'''UNABLE TO FORMAT,
  |${mkString(deepestYet.splits)}
  |'''.stripMargin
>>>
val msg = s'''UNABLE TO FORMAT,
             |${mkString(deepestYet.splits)}
             |'''.stripMargin
<<< SKIP olafurpg example #192
{
  def log(formatToken: FormatToken): String = s'''${log(formatToken.left)}
  |${log(formatToken.between:_*)}
  |${log(formatToken.right)}'''.stripMargin
}
>>>
{
  def log(formatToken: FormatToken): String = s'''${log(formatToken.left)}
                                                 |${log(formatToken.between: _*)}
                                                 |${log(formatToken.right)}'''.stripMargin
}
<<< pipe char on first line #324
s'''|first
        |$second'''.stripMargin
>>>
s'''|first
    |$second'''.stripMargin
<<< don't break if no need
val msg = s'''AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  |${mkString(deepestYet.splits)}
  |'''.stripMargin
>>>
val msg = s'''AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
             |${mkString(deepestYet.splits)}
             |'''.stripMargin""".replace("'''", "\"\"\"")

  override val tests =
    parseDiffTests(rawStrings, "stripMargin/String.stat") ++
      (parseDiffTests(interpolatedStrings, "stripMargin/Interpolate.stat"))

  testsToRun.foreach(runTest(run))

  val alignStyle =
    ScalafmtStyle.default.copy(assumeStandardLibraryStripMargin = true)

  def run(t: DiffTest, parse: Parse[_ <: Tree]): Unit = {
    val runner = scalafmtRunner.withParser(parse)
    val formatted = Scalafmt.format(t.original, alignStyle, runner).get
    saveResult(t, formatted, t.only)
    assertNoDiff(formatted, t.expected)
    assertNoDiff(Scalafmt.format(formatted, alignStyle, runner).get,
                 formatted,
                 "Idempotency violated")
  }
}
