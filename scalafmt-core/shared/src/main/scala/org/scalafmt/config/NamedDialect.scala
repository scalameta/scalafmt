package org.scalafmt.config

import metaconfig.ConfCodecEx

import scala.meta.Dialect
import scala.meta.dialects._

case class NamedDialect(name: String, dialect: Dialect)

object NamedDialect {

  def apply(pair: sourcecode.Text[Dialect]): NamedDialect = {
    val name = pair.source.substring(pair.source.lastIndexOf('.') + 1)
    apply(name.toLowerCase, pair.value)
  }

  val scala212 = Scala212.withAllowTrailingCommas(true) // SIP-27, 2.12.2
  val scala213 = Scala213.withAllowTrailingCommas(true)
  val scala3 = Scala3

  private[config] val known = Seq[sourcecode.Text[Dialect]](
    Scala211,
    scala212,
    Scala212Source3,
    scala213,
    Scala213Source3,
    Sbt0137,
    Sbt1,
    Scala3Future,
    scala3
  ).map(apply).sortBy(_.name)

  private[config] val defaultName = "default"
  // current default is 213
  private[config] val default = apply(defaultName, scala213)

  def getName(dialect: Dialect): Option[String] = known
    .find(_.dialect eq dialect).map(_.name)

  def getUnknownError = {
    val knownStr = known.map(_.name).mkString(",")
    s"""|Default dialect is deprecated; use explicit: [$knownStr]
        |Also see https://scalameta.org/scalafmt/docs/configuration.html#scala-dialects"
        |""".stripMargin
  }

  implicit val codec: ConfCodecEx[NamedDialect] = ReaderUtil
    .oneOf(known.map(x => sourcecode.Text(x, x.name)): _*)

}
