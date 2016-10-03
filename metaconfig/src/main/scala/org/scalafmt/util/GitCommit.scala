package org.scalafmt.util

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("Gets expanded away")
class GitCommit extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case defn @ Defn.Val(mods, pats, optType, rhs) if pats.size == 1 =>
        import sys.process._
        val commit = scala.util
          .Try(Seq("git", "rev-parse", "HEAD").!!.trim)
          .map(_.take(10))
          .getOrElse("UNKNOWN")
        val expr = Lit(commit)
        defn.copy(rhs = expr)
      case _ =>
        abort("This annotation should be used as follows: `@GitCommit val gitCommit: String = ???`")
    }
  }

}
