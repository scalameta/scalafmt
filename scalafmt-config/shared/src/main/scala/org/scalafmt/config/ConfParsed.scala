package org.scalafmt.config

import java.nio.file.Path

import scala.io.Codec
import scala.util.Try

import metaconfig._
import org.scalafmt.config.PlatformConfig._

class ConfParsed(val conf: Configured[Conf]) extends AnyVal {

  def getHoconValueOpt[A](
      path: String*
  )(implicit ev: ConfDecoderEx[A]): Option[Either[String, A]] = {
    val res = conf.andThen(_.getNestedConf(path: _*)).andThen(ev.read(None, _))
    res.fold[Option[Either[String, A]]](x =>
      if (x.isMissingField) None else Some(Left(x.msg))
    )(x => Some(Right(x)))
  }

  def isGit: Option[Either[String, Boolean]] =
    getHoconValueOpt[Boolean]("project", "git")

  def fatalWarnings: Option[Either[String, Boolean]] =
    getHoconValueOpt[Boolean]("runner", "fatalWarnings")

  def ignoreWarnings: Option[Either[String, Boolean]] =
    getHoconValueOpt[Boolean]("runner", "ignoreWarnings")

  def onTestFailure: Option[Either[String, String]] =
    getHoconValueOpt[String]("onTestFailure")

  def encoding: Option[Either[String, Codec]] =
    getHoconValueOpt[String]("encoding").map {
      _.flatMap(x => Try(Codec(x)).toEither.left.map(_.getMessage))
    }

  def version: Option[Either[String, String]] =
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
