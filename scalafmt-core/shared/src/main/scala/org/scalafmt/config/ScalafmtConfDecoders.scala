package org.scalafmt.config

import scala.io.Codec
import scala.meta.parsers.Parse._

import metaconfig.Conf
import metaconfig.ConfCodecEx
import metaconfig.ConfDecoderEx
import metaconfig.Configured

object ScalafmtConfDecoders extends ScalafmtConfDecoders

trait ScalafmtConfDecoders {
  type FormatEventCb = FormatEvent => Unit
  implicit lazy val eventReader: ConfDecoderEx[FormatEventCb] = ConfDecoderEx
    .from[FormatEventCb] { case _ => Configured.Ok((_: FormatEvent) => ()) }
  implicit lazy val parseReader: ConfCodecEx[MetaParser] = ReaderUtil
    .oneOf[MetaParser](parseSource, parseStat, parseCase)

  implicit lazy val codecReader: ConfDecoderEx[Codec] = ConfDecoderEx
    .fromPartial[Codec]("String") { case (_, Conf.Str(s)) =>
      Configured.fromExceptionThrowing(Codec(s))
    }

}
