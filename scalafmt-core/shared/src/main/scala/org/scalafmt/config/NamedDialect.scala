package org.scalafmt.config

import scala.meta.Dialect
import scala.meta.dialects._

import metaconfig.ConfCodecEx

object NamedDialect {

  def apply(nd: NamedDialect): NamedDialect = nd

  def apply(name: String, dialect: Dialect): NamedDialect =
    new NamedDialect(dialect, name)

  private[config] val known = Seq[NamedDialect](
    Scala211,
    Scala212,
    Scala212Source3,
    Scala213,
    Scala213Source3,
    Sbt0137,
    Sbt1,
    Scala3Future,
    Scala3,
    Scala30,
    Scala31,
    Scala32,
    Scala33,
    Scala34,
    Scala35,
    Scala36,
    Scala37,
    Scala38,
  ).sortBy(_.source)

  private[config] val defaultName = "default"
  // current default is 213
  private[config] val default = apply(defaultName, Scala213)

  def getName(dialect: Dialect): Option[String] = known.find(_.value eq dialect)
    .map(_.source)

  def getUnknownError = {
    val knownStr = known.map(_.source).mkString(",")
    s"""|Default dialect is deprecated; use explicit: [$knownStr]
        |Also see https://scalameta.org/scalafmt/docs/configuration.html#scala-dialects"
        |""".stripMargin
  }

  implicit val codec: ConfCodecEx[NamedDialect] = ReaderUtil
    .oneOf(known.map(nd => sourcecode.Text(nd, nd.source)): _*)

}
