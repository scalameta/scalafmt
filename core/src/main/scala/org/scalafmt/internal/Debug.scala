package org.scalafmt.internal

import java.util.concurrent.TimeUnit

import scala.collection.mutable
import scala.meta.Tree
import scala.meta.tokens.Token

/**
  * (ugly) Utility to collect data about formatter.
  *
  * Only used during development.
  */
object Debug extends ScalaFmtLogger {
  val treeExplored = mutable.Map.empty[Tree, Int]
  val tokenExplored = mutable.Map.empty[Token, Int]
  val formatTokenExplored = mutable.Map.empty[FormatToken, Int]
  val enqueuedSplits = mutable.Set.empty[Split]
  var lastTestExplored = 0
  var explored = 0
  var state = State.start
  var tokens = Array.empty[FormatToken]
  var startTime = System.nanoTime()

  def newTest(): Unit = {
    treeExplored.clear()
    tokenExplored.clear()
    startTime = System.nanoTime()
    lastTestExplored = explored
  }

  def ns2ms(nanoseconds: Long): Long =
    TimeUnit.MILLISECONDS.convert(nanoseconds, TimeUnit.NANOSECONDS)

  def elapsedNs = System.nanoTime() - startTime

  def exploredInTest = explored - lastTestExplored

  def maxVisitedToken: Int = {
    if (tokens.isEmpty) 0
    else {
      val maxTok = tokens.maxBy(x => formatTokenExplored.getOrElse(x, 0))
      formatTokenExplored.getOrElse(maxTok, -1)
    }
  }

  def enqueued(split: Split): Unit = {
    enqueuedSplits += split
  }

  def visit(token: FormatToken): Unit = {
    visit(token.left)
    visit(token.right)
    val visits = formatTokenExplored.getOrElse(token, 0) + 1
    formatTokenExplored += token -> visits
  }

  def visit(token: Token): Unit = {
    val visits = tokenExplored.getOrElse(token, 0) + 1
    tokenExplored += token -> visits
  }

  def visit(tree: Tree): Unit = {
    val visits = treeExplored.getOrElse(tree, 0) + 1
    treeExplored += tree -> visits
  }
}