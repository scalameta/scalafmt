package org.scalafmt.readme

import scalatags.Text.TypedTag
import scalatags.Text.all._

import java.text.SimpleDateFormat
import java.util.Date

import com.twitter.util.Eval
import org.scalafmt.Scalafmt
import org.scalafmt.cli.Cli
import org.scalafmt.cli.CliArgParser
import org.scalafmt.config.AlignToken
import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtRunner
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.rewrite._

object hl extends scalatex.site.Highlighter

object Readme {

  val eval = new Eval()
  implicit def bool2frag(boolean: Boolean): StringFrag =
    stringFrag(boolean.toString)

  def flag(str: String) = {
    println(str)
    require(Config.fromHocon(str).isRight)
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

  def cliFlags(flags: String) = {
    require(Config.fromHocon(flags).isRight, s"Invalid configuration: $flags")
    hl.scala(flags)
  }

  def note = b("NOTE")

  def github: String = "https://github.com"
  def repo: String = "https://github.com/olafurpg/scalafmt"

  def user(name: String) = a(href := s"$github/$name", s"@$name")
  def users(names: String*) =
    span(
      names.dropRight(1).map(x => span(user(x), ", ")) :+ user(names.last): _*
    )

  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")
  def issues(ids: Int*) = span(ids.map(issue): _*)

  def half(frags: Frag*) = div(frags, width := "50%", float.left)

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

  val rewriteBraces =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
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

  val rewritePreferCurlyFors =
    ScalafmtConfig.default.copy(
      rewrite = ScalafmtConfig.default.rewrite.copy(
        rules = Seq(PreferCurlyFors)
      ))

  def fmt(style: ScalafmtConfig)(code: String): TypedTag[String] =
    example(code, style)

  def lastUpdated =
    new SimpleDateFormat("MMM d, y").format(new Date(CliArgParser.buildTimeMs))

  def example(code: String, style: ScalafmtConfig): TypedTag[String] = {
    val formatted = Scalafmt.format(code, style).get
    hl.scala(formatted)
  }
}
