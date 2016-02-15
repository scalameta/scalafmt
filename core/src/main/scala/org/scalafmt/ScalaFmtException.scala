package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token.Keyword

import scala.reflect.ClassTag
import scala.reflect.classTag

class ScalaFmtException(msg: String) extends Exception(msg)

case class CantFindDefnToken[T <: Keyword: ClassTag](tree: Tree)
  extends ScalaFmtException(
    s"Expected keyword of type ${classTag[T].getClass} in tree $tree")
case object TooManyIndentPops extends ScalaFmtException("Too many Indent Pop.")
case object CantFormatFile
  extends ScalaFmtException("scalafmt cannot format this file")