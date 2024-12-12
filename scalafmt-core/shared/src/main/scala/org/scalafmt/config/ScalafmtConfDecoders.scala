package org.scalafmt.config

import scala.meta.parsers.Parse._

import scala.io.Codec

import metaconfig._

object ScalafmtConfDecoders extends ScalafmtConfDecoders

trait ScalafmtConfDecoders {
  implicit def eventReaderFor[A <: FormatEvent]: ConfDecoderEx[A => Unit] =
    ConfDecoderEx.from[A => Unit] { case _ => Configured.Ok((_: A) => ()) }

  implicit lazy val parseReader: ConfCodecEx[MetaParser] = ReaderUtil
    .oneOf[MetaParser](parseSource, parseStat, parseCase)

  implicit lazy val codecReader: ConfDecoderEx[Codec] = ConfDecoderEx
    .fromPartial[Codec]("String") { case (_, Conf.Str(s)) =>
      Configured.fromExceptionThrowing(Codec(s))
    }

}
