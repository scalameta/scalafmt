package org.scalafmt

import scala.collection.mutable
import scala.meta.Tree

object Debug extends ScalaFmtLogger {
  val treeExplored = mutable.Map.empty[Tree, Int]
  val tokenExplored = mutable.Map.empty[FormatToken, Int]
  var explored = 0

  def clear(): Unit = {
    treeExplored.clear()
    tokenExplored.clear()
  }

  def visit(token: FormatToken): Unit = {
    val visits = tokenExplored.getOrElse(token, 0) + 1
    tokenExplored += token -> visits
  }

  def reportTokens = {
    tokenExplored.toSeq.sortBy(_._1.right.end).foreach {
      case (code, count) =>
        logger.debug(
          f"""$count%-5s $code""".stripMargin)
    }
  }

  def reportTrees = {
    Debug.treeExplored.toSeq.sortBy(_._2).foreach { case (code, count) =>
      logger.debug(
        f"""$count%-5s
           |$code""".stripMargin)
    }
  }

  def visit(tree: Tree): Unit = {
    val visits = treeExplored.getOrElse(tree, 0) + 1
    treeExplored += tree -> visits
  }
}
