package org.scalafmt.config

import scala.language.reflectiveCalls

import scala.collection.IterableLike
import scala.meta.internal.parsers.ScalametaParser
import scala.util.Try

import metaconfig.HasFields
import metaconfig.Result
import metaconfig.String2AnyMap
import org.scalafmt.config.hocon.Hocon2Class
import org.scalafmt.util.logger

object Config {

  def displayAll(any: Any): Seq[String] = any match {
    case String2AnyMap(m) =>
      m.flatMap {
        case (k, v) =>
          displayAll(v).map { x =>
            if (x.startsWith(" ")) s"$k$x"
            else s"$k.$x"
          }
      }.toSeq
    case x: HasFields => displayAll(x.fields)
    case x: Traversable[_] =>
      Seq(x.flatMap(displayAll).mkString(" = [", "\n  ", "]"))
    case x =>
      val str = s"$x"
      val output =
        if (str.headOption.exists(!_.isLetterOrDigit)) s""""$str""""
        else str
      Seq(s" = $output")
  }

  /** Read ScalafmtConfig from String contents from an optional HOCON path. */
  def fromHocon(string: String,
                path: Option[String] = None): Result[ScalafmtConfig] =
    Hocon2Class
      .gimmeClass[ScalafmtConfig](string, ScalafmtConfig.configReader, path)

}
