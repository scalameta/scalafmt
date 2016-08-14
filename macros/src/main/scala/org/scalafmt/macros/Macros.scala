package org.scalafmt.macros

import scala.language.experimental.macros

import scala.reflect.macros.Context

object Macros {
  def gitCommit: String = macro gitCommitImpl

  def gitCommitImpl(c: Context): c.Expr[String] = {
    import sys.process._
    import c.universe._
    val commit = scala.util
      .Try(Seq("git", "rev-parse", "HEAD").!!.trim)
      .map(_.take(10))
      .getOrElse("UNKNOWN")

    c.Expr(Literal(Constant(commit)))
  }

  def buildTimeMs: Long = macro buildTimeImpl

  def buildTimeImpl(c: Context): c.Expr[Long] = {
    import c.universe._
    val timestamp = System.currentTimeMillis()
    c.Expr(Literal(Constant(timestamp)))
  }
}
