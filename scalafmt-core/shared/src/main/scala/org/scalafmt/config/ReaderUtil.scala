package org.scalafmt.config

import scala.reflect.ClassTag

import metaconfig._
import metaconfig.Configured._

object ReaderUtil {

  private def lowerCaseNoBackticks(s: String): String =
    s.toLowerCase().replace("`", "")

  // Poor mans coproduct reader
  def oneOf[T: ClassTag](options: sourcecode.Text[T]*): ConfCodec[T] = {
    oneOfCustom(options: _*)(PartialFunction.empty)
  }

  def oneOfCustom[T: ClassTag](
      options: sourcecode.Text[T]*
  )(f: PartialFunction[Conf, Configured[T]]): ConfCodec[T] = {
    val m = options.map(x => lowerCaseNoBackticks(x.source) -> x.value).toMap
    val decoder = ConfDecoder.instance[T](f.orElse {
      case Conf.Str(x) =>
        m.get(lowerCaseNoBackticks(x)) match {
          case Some(y) =>
            Ok(y)
          case None =>
            val available = m.keys.mkString(", ")
            val msg = s"Unknown input '$x'. Expected one of: $available"
            ConfError.message(msg).notOk
        }
    })
    val encoder = ConfEncoder.instance[T] { value =>
      options
        .collectFirst {
          case sourcecode.Text(`value`, source) => Conf.Str(source)
        }
        .getOrElse(Conf.Null())
    }
    ConfCodec.EncoderDecoderToCodec(encoder, decoder)
  }

}
