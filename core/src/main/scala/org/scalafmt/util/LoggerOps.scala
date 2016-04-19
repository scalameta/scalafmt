package org.scalafmt.util

import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Split
import scala.meta.Tree
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Interpolation
import scala.meta.tokens.Tokens

/**
  * Debugging utility.
  */
object LoggerOps {
  val logger = PrintlnLogger

  def log(split: Split): String = s"$split"

  def log(formatToken: FormatToken): String =
    s"""${log(formatToken.left)}
       |${log(
           formatToken.between: _*)}
       |${log(formatToken.right)}""".stripMargin

  def escape(raw: String): String = {
    raw
  }

  def log(tokens: Token*): String = tokens.map(log).mkString("\n")

  def cleanup(token: Token): String = token match {
    case _: Token.Literal | _: Interpolation.Part =>
      escape(token.code).stripPrefix("\"").stripSuffix("\"")
    case _ => token.code.replace("\n", "")
  }

  def log(tokens: Tokens): String = tokens.map(log).mkString("\n")

  def log(token: Token): String =
    f"${cleanup(token).slice(0, 30)}%-30s ${getTokenClass(token)}"

  private def getTokenClass(token: Token) =
    token.getClass.getName.stripPrefix("scala.meta.tokens.Token$")

  def log(t: Tree, tokensOnly: Boolean = false): String = {
    val tokens = s"TOKENS: ${t.tokens.map(x => reveal(x.code)).mkString(",")}"
    if (tokensOnly) tokens
    else s"""TYPE: ${t.getClass.getName.stripPrefix("scala.meta.")}
            |SOURCE: $t
            |STRUCTURE: ${t.show[Structure]}
            |$tokens
            |""".stripMargin
  }

  def reveal(s: String): String = s.map {
    case '\n' => '¶'
    case ' ' => '∙'
    case ch => ch
  }

  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }
}
