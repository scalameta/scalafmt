package org.scalafmt.readme

import scalatags.Text.TypedTag
import scalatags.Text.all._
import java.text.SimpleDateFormat
import java.util.Date
import com.twitter.util.Eval
import org.scalafmt.Scalafmt
import org.scalafmt.cli.Cli
import org.scalafmt.cli.CliArgParser
import org.scalafmt.config._
import org.scalafmt.rewrite._

object hl extends scalatex.site.Highlighter

object Readme {

  def gitter = a(
    href := "https://gitter.im/scalameta/scalafmt",
    img(
      src := "https://camo.githubusercontent.com/da2edb525cde1455a622c58c0effc3a90b9a181c/68747470733a2f2f6261646765732e6769747465722e696d2f4a6f696e253230436861742e737667",
      alt := "Join the chat at https://gitter.im/scalameta/scalameta",
      maxWidth := "100%;"
    )
  )

  val eval = new Eval()
  implicit def bool2frag(boolean: Boolean): StringFrag =
    stringFrag(boolean.toString)

  def flag(str: String) = {
    println(str)
    Config.fromHoconString(str).get
    code(str)
  }

  /**
    * repl session, inspired by tut.
    *
    * Example: code="1 + 1" returns
    * "scala> 1 + 1
    * res0: Int = 2"
    */
  def repl(code: String) = {
    import scala.meta._
    val expressions = s"{$code}".parse[Stat].get.asInstanceOf[Term.Block].stats
    val evaluated = eval[Any](code)
    val output = evaluated match {
      case s: String =>
        s"""
           |"$s"""".stripMargin
      case x => x.toString
    }
    val result = s"""${expressions
                      .map(x => s"scala> ${x.toString().trim}")
                      .mkString("\n")}
                    |res0: ${evaluated.getClass.getName} = $output
                    |""".stripMargin
    hl.scala(result)
  }

  def config(frags: Frag*) = {
    cliFlags(frags.render)
  }
  def cliFlags(flags: String) = {
    Config.fromHoconString(flags).get
    hl.scala(flags)
  }

  def note = b("NOTE")

  def github: String = "https://github.com"
  def repo: String = "https://github.com/scalameta/scalafmt"
  def gitRepo: String = repo + ".git"

  def user(name: String) = a(href := s"$github/$name", s"@$name")
  def users(names: String*) =
    span(
      names.dropRight(1).map(x => span(user(x), ", ")) :+ user(names.last): _*
    )

  def pr(id: Int) = a(href := repo + s"/pull/$id", s"#$id")
  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")
  def issues(ids: Int*) = span(ids.map(issue): _*)

  def half(frags: Frag*) = div(frags, width := "50%", float.left)
  def ignore(frags: Frag*) = span("")

  def pairs(frags: Frag*) = div(frags, div(clear := "both"))

  def sideBySide(left: String, right: String) =
    pairs(List(left, right).map(x => half(hl.scala(x))): _*)

  def demo(code: String) = {
    val formatted = Scalafmt.format(code, ScalafmtConfig.default40).get
    sideBySide(code, formatted)
  }

  def demoStyle(style: ScalafmtConfig)(code: String) = {
    val formatted =
      Scalafmt.format(code, style.copy(runner = ScalafmtRunner.sbt)).get
    sideBySide(code, formatted)
  }

  def example(code: String): TypedTag[String] = {
    example(code, ScalafmtConfig.default40)
  }

  def exampleAlign(code: String): TypedTag[String] = {
    val formatted = Scalafmt
      .format(
        code,
        ScalafmtConfig.default40.copy(
          align =
            ScalafmtConfig.default40.align.copy(tokens = AlignToken.default)))
      .get
    hl.scala(formatted)
  }

  val stripMarginStyle =
    ScalafmtConfig.default.copy(assumeStandardLibraryStripMargin = true)

  val rewriteInfix =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(AvoidInfix)
      ))

  val rewriteImportSelectors =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(ExpandImportSelectors)
      ))

  val rewriteBraces =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        redundantBraces = ScalafmtConfig.default.rewrite.redundantBraces.copy(
          stringInterpolation = true
        ),
        rules = Seq(RedundantBraces)
      ))

  val rewriteParens =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(RedundantParens)
      ))

  val rewriteImports =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(SortImports)
      ))

  val rewriteAsciiImports =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(AsciiSortImports)
      ))

  val rewritePreferCurlyFors =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(PreferCurlyFors)
      ))

  def fmt(style: ScalafmtConfig)(code: String): TypedTag[String] =
    example(code, style)

  def lastUpdated =
    new SimpleDateFormat("MMM d, y").format(new Date(CliArgParser.buildTimeMs))

  def format(code: String): TypedTag[String] = {
    format(ScalafmtConfig.default)(code)
  }

  val alignNone = ScalafmtConfig.default.copy(align = Align.none)
  val alignSome = ScalafmtConfig.default.copy(align = Align.some)
  val alignMore = ScalafmtConfig.default.copy(align = Align.more)
  val alignMost = ScalafmtConfig.default.copy(align = Align.most)
  val alignCaseArrow = ScalafmtConfig.default
  val alignArrowEnum = ScalafmtConfig.defaultWithAlign.copy(
    align = Align.default.copy(arrowEnumeratorGenerator = true))
  val alignModuleId = ScalafmtConfig.defaultWithAlign

  def format(style: ScalafmtConfig)(code: String): TypedTag[String] = {
    example(code, style.copy(runner = ScalafmtRunner.sbt))
  }

  def example(code: String, style: ScalafmtConfig): TypedTag[String] = {
    val formatted = Scalafmt.format(code, style).get
    hl.scala(formatted)
  }
  def image(url: String) = img(
    src := url,
    width := "100%",
    textAlign := "center"
  )
}
