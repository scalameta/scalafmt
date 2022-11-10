package org.scalafmt.util

import scala.meta.Tree
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Token.Interpolation
import scala.meta.tokens.Tokens

import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Split
import org.scalafmt.internal.State

import sourcecode.Text

/** Debugging utility.
  */
object LoggerOps {
  val logger = org.scalameta.logger

  // TODO(olafur) parameterize
  def name2style[T](styles: Text[T]*): Map[String, T] =
    styles.map(x => x.source -> x.value).toMap

  def log(s: State): String = {
    val policies = s.policy.policies.map(_.toString).mkString("P[", ",", "]")
    s"d=${s.depth} w=${s.cost} i=${s.indentation} col=${s.column}; $policies; s=${log(s.split)}"
  }
  def log(split: Split): String = s"$split"

  def log(formatToken: FormatToken): String =
    s"""${log(formatToken.left)}
      |${log(formatToken.between: _*)}
      |${log(formatToken.right)}""".stripMargin

  def log2(formatToken: FormatToken): String = formatToken.toString

  def escape(raw: String): String = {
    raw
  }

  def log(tokens: Token*): String = tokens.map(log).mkString("\n")

  def cleanup(token: Token): String =
    token match {
      case Literal() | Interpolation.Part(_) =>
        escape(token.syntax).stripPrefix("\"").stripSuffix("\"")
      case _ => token.syntax.replace("\n", "")
    }

  def log(tokens: Tokens): String = tokens.map(log).mkString("\n")

  def log(token: Token): String =
    f"[${token.end}]${cleanup(token).slice(0, 30)}%-30s ${getTokenClass(token)}"

  private def getTokenClass(token: Token) =
    token.getClass.getName.stripPrefix("scala.meta.tokens.Token$")

  def log(t: Tree): String = log(t, false)
  def log(t: Tree, tokensOnly: Boolean): String = {
    val tokens =
      s"TOKENS: ${t.tokens.map(x => reveal(x.syntax)).mkString(",")}"
    if (tokensOnly) tokens
    else s"""TYPE: ${t.getClass.getName.stripPrefix("scala.meta.")}
      |SOURCE: $t
      |STRUCTURE: ${t.show[Structure]}
      |$tokens
      |""".stripMargin
  }

  def log(t: Option[Tree]): String = log(t, false)
  def log(t: Option[Tree], tokensOnly: Boolean): String =
    t.fold("")(log(_, tokensOnly))

  def reveal(s: String): String =
    s.map {
      case '\n' => '¶'
      case ' ' => '∙'
      case ch => ch
    }

  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }
}
