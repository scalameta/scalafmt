package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token.Keyword
import scala.reflect.ClassTag
import scala.reflect.classTag

sealed abstract class Error(msg: String) extends Exception(msg)

object Error {
    case class CantFindDefnToken[T <: Keyword : ClassTag](tree: Tree)
      extends Error(
          s"Expected keyword of type ${classTag[T].getClass} in tree $tree")

    case object TooManyIndentPops extends Error("Too many Indent Pop.")

    case object CantFormatFile
      extends Error("scalafmt cannot format this file")
}
