package org.scalafmt.config

import scala.io.Codec
import scala.meta.parsers.Parse._

import metaconfig.Conf
import metaconfig.ConfDecoder
import metaconfig.Configured

object ScalafmtConfDecoders extends ScalafmtConfDecoders

trait ScalafmtConfDecoders {
  implicit lazy val eventReader: ConfDecoder[FormatEvent => Unit] =
    ConfDecoder.from[FormatEvent => Unit] { case _ =>
      Configured.Ok((_: FormatEvent) => ())
    }
  implicit lazy val parseReader: ConfDecoder[MetaParser] = {
    ReaderUtil.oneOf[MetaParser](parseSource, parseStat, parseCase)
  }

  implicit lazy val codecReader: ConfDecoder[Codec] =
    ConfDecoder.fromPartial("String") { case Conf.Str(s) =>
      Configured.fromExceptionThrowing(Codec(s))
    }

}
