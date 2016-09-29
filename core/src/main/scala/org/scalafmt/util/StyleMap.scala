package org.scalafmt.util

import scala.meta.tokens.Token
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Tokens

import org.scalafmt.config.Config
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken

class StyleMap(tokens: Array[FormatToken], init: ScalafmtConfig) {
  private val prefix = "\\s*scalafmt: ".r
  val (isEmpty: Boolean, tok2style: Map[FormatToken, ScalafmtConfig]) = {
    var curr = init
    var empty = true
    val map = Map.newBuilder[FormatToken, ScalafmtConfig]
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
    (empty, map.result())
  }

  def at(token: FormatToken): ScalafmtConfig = {
    tok2style.getOrElse(token, init)
  }

}
