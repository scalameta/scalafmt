package org.scalafmt

import com.typesafe.scalalogging.Logger
import org.slf4j.LoggerFactory

import scala.meta.Tree
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Tokens

trait ScalaFmtLogger {
  val logger = Logger(LoggerFactory.getLogger(this.getClass))

  private def getTokenClass(token: Token) =
    token.getClass.getName.stripPrefix("scala.meta.tokens.Token$")

  def log(token: Token): String = f"$token%30s ${getTokenClass(token)}"
  def log(tokens: Token*): String = tokens.map(log).mkString("\n")
  def log(tokens: Tokens): String = tokens.map(log).mkString("\n")

  def header[T](t: T): String = {
    val line = s"=" * (t.toString.length + 3)
    s"$line\n=> $t\n$line"
  }

  def reveal(s: String): String =
    s.replaceAll("\n", "¶")
      .replaceAll(" ", "∙")



  def log(t: Tree, line: Int): Unit = {
    logger.debug(
      s"""${header(line)}
         |TYPE: ${t.getClass.getName.stripPrefix("scala.meta.")}
         |SOURCE: $t
         |STRUCTURE: ${t.show[Structure]}
         |TOKENS: ${t.tokens.map(x => reveal(x.code)).mkString(",")}
         |""".stripMargin)
  }
}

