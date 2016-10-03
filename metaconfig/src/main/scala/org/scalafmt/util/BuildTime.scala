package org.scalafmt.util

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("Gets expanded away")
class BuildTime extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case defn @ Defn.Val(mods, pats, optType, rhs) if pats.size == 1 =>
        defn.copy(rhs = Lit(System.currentTimeMillis()))
      case _ =>
        abort("This annotation should be used as follows: `@BuildTime val buildTimeMs: Long = ???`")
    }
  }

}
