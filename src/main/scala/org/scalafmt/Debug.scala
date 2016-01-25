package org.scalafmt

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.tokens.Token

object Debug extends ScalaFmtLogger {
  val treeExplored = mutable.Map.empty[Tree, Int]
  val tokenExplored = mutable.Map.empty[Token, Int]
  val formatTokenExplored = mutable.Map.empty[FormatToken, Int]
  var explored = 0
  var state = State.start
  var toks = Array.empty[FormatToken]

  def clear(): Unit = {
    treeExplored.clear()
    tokenExplored.clear()
  }

  def visit(token: Token): Unit = {
    val visits = tokenExplored.getOrElse(token, 0) + 1
    tokenExplored += token -> visits
  }

  def visit(token: FormatToken): Unit = {
    visit(token.left)
    visit(token.right)
    val visits = formatTokenExplored.getOrElse(token, 0) + 1
    formatTokenExplored += token -> visits
  }

  def visit(tree: Tree): Unit = {
    val visits = treeExplored.getOrElse(tree, 0) + 1
    treeExplored += tree -> visits
  }

  def reportTokens() = {
    formatTokenExplored.toSeq.sortBy(_._1.right.end).foreach {
      case (code, count) =>
        logger.debug(
          f"""$count%-5s $code""".stripMargin)
    }
  }

  def reportTrees() = {
    Debug.treeExplored.toSeq.sortBy(_._2).foreach { case (code, count) =>
      logger.debug(
        f"""$count%-5s
           |$code""".stripMargin)
    }
  }

}
