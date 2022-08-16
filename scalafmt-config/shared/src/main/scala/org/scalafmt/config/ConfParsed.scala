package org.scalafmt.config

import java.nio.file.Path

import scala.io.Codec
import scala.util.Try

import metaconfig._
import org.scalafmt.config.PlatformConfig._

class ConfParsed(val conf: Configured[Conf]) extends AnyVal {

  def getHoconValueOpt[A](
      path: String*
  )(implicit ev: ConfDecoderEx[A]): Option[A] = {
    val nestedConf = conf.andThen(_.getNestedConf(path: _*))
    nestedConf.andThen(ev.read(None, _)).map(Some(_)).getOrElse(None)
  }

  def getHoconValue[A: ConfDecoderEx](default: A, path: String*): A =
    getHoconValueOpt[A](path: _*).getOrElse(default)

  def isGit: Option[Boolean] =
    getHoconValueOpt[Boolean]("project", "git")

  def fatalWarnings: Option[Boolean] =
    getHoconValueOpt[Boolean]("runner", "fatalWarnings")

  def ignoreWarnings: Option[Boolean] =
    getHoconValueOpt[Boolean]("runner", "ignoreWarnings")

  def onTestFailure: Option[String] =
    getHoconValueOpt[String]("onTestFailure")

  def encoding: Option[Codec] =
    getHoconValueOpt[String]("encoding").flatMap(x => Try(Codec(x)).toOption)

  def version: Option[String] =
    getHoconValueOpt[String]("version")

}

object ConfParsed {

  def apply(input: Configured[Input], path: Option[String] = None): ConfParsed =
    new ConfParsed(input.andThen(_.parse(path)))

  def fromInput(input: Input, path: Option[String] = None): ConfParsed =
    apply(Configured.Ok(input), path)

  def fromString(input: String, path: Option[String] = None): ConfParsed =
    fromInput(Input.String(input), path)

  def fromPath(input: Path, path: Option[String] = None): ConfParsed =
    apply(Configured.fromExceptionThrowing(Input.File(input)), path)

}
