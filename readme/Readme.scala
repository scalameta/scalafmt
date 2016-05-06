package org.scalafmt.readme

import scalatags.Text.TypedTag
import scalatags.Text.all._

import com.twitter.util.Eval
import org.scalafmt.AlignToken
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtStyle

object hl extends scalatex.site.Highlighter

object Readme {

  val eval = new Eval()

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

  def note = b("NOTE")

  def repo: String = "https://github.com/olafurpg/scalafmt"

  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")

  def issues(ids: Int*) = span(ids.map(issue):_*)

  def half(frags: Frag*) = div(frags, width := "50%", float.left)

  def pairs(frags: Frag*) = div(frags, div(clear := "both"))

  def sideBySide(left: String, right: String) =
    pairs(List(left, right).map(x => half(hl.scala(x))):_*)

  def demo(code: String) = {
    import org.scalafmt._
    val formatted = Scalafmt.format(code, ScalafmtStyle.default40).get
    sideBySide(code, formatted)
  }

  def example(code: String): TypedTag[String] = {
    example(code, ScalafmtStyle.default40)
  }

  def exampleAlign(code: String): TypedTag[String] = {
    val formatted = Scalafmt
      .format(
          code, ScalafmtStyle.default40.copy(alignTokens = AlignToken.default))
      .get
    hl.scala(formatted)
  }

  def example(code: String, style: ScalafmtStyle): TypedTag[String] = {
    val formatted = Scalafmt.format(code, style).get
    hl.scala(formatted)
  }
}
