package org.scalafmt.util

import scala.util.Success
import scala.util.Try

import mdoc.parser._

private[scalafmt] object MarkdownParser {

  private val settings: ParserSettings = new ParserSettings {
    override val allowCodeFenceIndented: Boolean = true
  }

  def transformMdoc(code: String)(fmt: String => Try[String]): Try[String] = {
    var hadFencedParts = false
    val parts = MarkdownPart.parse(code, settings)
    parts.foreach {
      case p: CodeFence if p.getMdocMode.isDefined =>
        fmt(p.body.value) match {
          case Success(b) => hadFencedParts = true; p.newBody = Some(b.trim)
          case failure => return failure // RETURNING!
        }
      case _ =>
    }

    Success(if (hadFencedParts) {
      val out = new StringBuilder()
      parts.foreach(_.renderToString(out))
      if (out.last != '\n') out.append('\n')
      out.toString()
    } else code)
  }

}
