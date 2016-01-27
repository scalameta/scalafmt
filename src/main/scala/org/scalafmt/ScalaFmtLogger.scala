package org.scalafmt

import java.util.concurrent.TimeUnit

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.meta.Tree
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

trait ScalaFmtLogger {
  val logger = Logger(LoggerFactory.getLogger(this.getClass))

  def log(split: Split): String = s"$split"

  def log(formatToken: FormatToken): String =
    s"""${log(formatToken.left)}
       |${log(formatToken.between: _*)}
       |${log(formatToken.right)}""".stripMargin

  def log(tokens: Token*): String = tokens.map(log).mkString("\n")

  def log(token: Token): String = f"$token%-30s ${getTokenClass(token)}"

  private def getTokenClass(token: Token) =
    token.getClass.getName.stripPrefix("scala.meta.tokens.Token$")

  def log(tokens: Tokens): String = tokens.map(log).mkString("\n")

  def log(t: Tree): String = {
    s"""TYPE: ${t.getClass.getName.stripPrefix("scala.meta.")}
        |SOURCE: $t
        |STRUCTURE: ${t.show[Structure]}
        |TOKENS: ${t.tokens.map(x => reveal(x.code)).mkString(",")}
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

