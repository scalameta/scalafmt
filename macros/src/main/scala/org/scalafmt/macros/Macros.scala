package org.scalafmt.macros

import scala.language.experimental.macros

import scala.reflect.macros.Context


object Macros {
  def gitCommit: Option[String] = macro gitCommitImpl

  def gitCommitImpl(c: Context): c.Expr[Option[String]] = {
    import sys.process._
    import c.universe._
    reify(
      scala.util.Try(Seq("git", "rev-parse", "HEAD").!!.trim).toOption
    )
  }

  def buildTimeMs: Long = macro buildTimeImpl

  def buildTimeImpl(c: Context): c.Expr[Long] = {
    import c.universe._
    val timestamp = System.currentTimeMillis()
    c.Expr(Literal(Constant(timestamp)))
  }
}