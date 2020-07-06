package org.scalafmt.config

import scala.io.Codec
import scala.meta.parsers.Parse._
import scala.util.control.NonFatal

import metaconfig.Conf
import metaconfig.ConfDecoder
import metaconfig.ConfError
import metaconfig.Configured
import metaconfig.Configured.Ok

object ScalafmtConfDecoders extends ScalafmtConfDecoders

trait ScalafmtConfDecoders {
  implicit lazy val eventReader: ConfDecoder[FormatEvent => Unit] =
    ConfDecoder.instance[FormatEvent => Unit] {
      case _ =>
        Configured.Ok((_: FormatEvent) => ())
    }
  implicit lazy val parseReader: ConfDecoder[MetaParser] = {
    ReaderUtil.oneOf[MetaParser](parseSource, parseStat, parseCase)
  }

  implicit lazy val codecReader: ConfDecoder[Codec] =
    ConfDecoder.instance[Codec] {
      case Conf.Str(s) =>
        try Ok(Codec(s))
        catch { case NonFatal(e) => ConfError.message(e.getMessage).notOk }
    }

}
