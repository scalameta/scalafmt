package org.scalafmt.util

import scala.annotation.compileTimeOnly
import scala.meta._

@compileTimeOnly("@BuildTime not expanded. Have you enabled the scala.meta paradise compiler plugin?")
class BuildTime extends scala.annotation.StaticAnnotation {

  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods val ${name: Pat.Var.Term}: $tpeopt = ???" =>
        q"..$mods val $name: Long = ${System.currentTimeMillis()}"
      case _ =>
        abort("This annotation should be used as follows: `@BuildTime val buildTimeMs: Long = ???`")
    }
  }

}
