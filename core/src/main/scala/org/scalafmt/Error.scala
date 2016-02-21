package org.scalafmt

import org.scalafmt.internal.ScalaFmtLogger

import scala.meta.Case
import scala.meta.Tree
import scala.meta.tokens.Token.Keyword
import scala.reflect.ClassTag
import scala.reflect.classTag

sealed abstract class Error(msg: String) extends Exception(msg)

object Error extends ScalaFmtLogger {
    case class CantFindDefnToken[T <: Keyword : ClassTag](tree: Tree)
      extends Error(
          s"Expected keyword of type ${classTag[T].getClass} in tree $tree")

    case object TooManyIndentPops extends Error("Too many Indent Pop.")

  case class CaseMissingArrow(tree: Case)
    extends Error(s"Missing => in case: \n$tree")

  case object FormatterChangedAST
    extends Error("Formatter changed AST")

  case class FormatterOutputDoesNotParse(msg: String)
    extends Error("Formatter output does not parse:\n" + msg)

  case class UnexpectedTree[Expected <: Tree : ClassTag](obtained: Tree)
    extends Error(
      s"Expected: ${classTag[Expected].getClass}\nObtained: ${log(obtained)}")

  case object CantFormatFile
    extends Error("scalafmt cannot format this file")


}
