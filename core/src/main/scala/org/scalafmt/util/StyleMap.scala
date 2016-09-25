package org.scalafmt.util

import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Tokens

import org.scalafmt.ScalafmtStyle
import org.scalafmt.config.Config
import org.scalafmt.internal.FormatToken

class StyleMap(tokens: Array[FormatToken], init: ScalafmtStyle) {
  private val prefix = "\\s*scalafmt: ".r
  val (isEmpty: Boolean, tok2style: Map[FormatToken, ScalafmtStyle]) = {
    var curr = init
    var empty = true
    val map = Map.newBuilder[FormatToken, ScalafmtStyle]
    tokens.foreach { tok =>
      tok.left match {
        case x @ Comment(c) if prefix.findFirstIn(c).isDefined =>
          Config.fromHocon(c, Some("scalafmt")) match {
            case Right(style) =>
              empty = false
              curr = style
            case Left(e) => // TODO(olafur) report error via callback
          }
        case _ =>
      }
      if (!empty) {
        // Minor optimization? Only add to map if needed.
        map += (tok -> curr)
      }
    }
    (empty, map.result().withDefaultValue(init))
  }

  def at(token: FormatToken): ScalafmtStyle = {
    tok2style(token)
  }

}
