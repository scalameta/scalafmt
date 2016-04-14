package org.scalafmt.readme

import scalatags.Text.TypedTag
import scalatags.Text.all._

import org.scalafmt.AlignToken
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtStyle

object hl extends scalatex.site.Highlighter

object Readme {

  def note = b("NOTE")

  def repo: String = "https://github.com/olafurpg/scalafmt"

  def issue(id: Int) = a(href := repo + s"/issues/$id", s"#$id")

  def issues(ids: Int*) = span(ids.map(issue):_*)

  def half(frags: Frag*) = div(frags, width := "50%", float.left)

  def pairs(frags: Frag*) = div(frags, div(clear := "both"))

  def demo(code: String) = {
    import org.scalafmt._
    val formatted = Scalafmt.format(code, ScalafmtStyle.default40).get
    pairs(List(code, formatted).map(x => half(hl.scala(x))):_*)
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
