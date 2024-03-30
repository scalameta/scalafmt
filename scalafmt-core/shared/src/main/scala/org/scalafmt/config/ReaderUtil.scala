package org.scalafmt.config

import scala.reflect.ClassTag

import metaconfig._

object ReaderUtil {

  private def lowerCaseNoBackticks(s: String): String = s.toLowerCase()
    .replace("`", "")

  // Poor mans coproduct reader
  def oneOf[T: ClassTag](options: sourcecode.Text[T]*): ConfCodecEx[T] = {
    oneOfCustomEx(options: _*)(PartialFunction.empty)
  }

  def oneOfCustom[T: ClassTag](
      options: sourcecode.Text[T]*
  )(f: PartialFunction[Conf, Configured[T]]): ConfCodecEx[T] = {
    object extract {
      def unapply(conf: Conf): Option[Configured[T]] = f.lift(conf)
    }
    oneOfCustomEx(options: _*) { case (_, extract(res)) => res }
  }

  def oneOfCustomEx[T: ClassTag](
      options: sourcecode.Text[T]*
  )(f: PartialFunction[(Option[T], Conf), Configured[T]]): ConfCodecEx[T] = {
    val m = options.map(x => lowerCaseNoBackticks(x.source) -> x.value).toMap
    val decoder = ConfDecoderEx.fromPartial[T]("String")(f.orElse {
      case (_, Conf.Str(x)) => Configured.opt(m.get(lowerCaseNoBackticks(x))) {
          val available = m.keys.mkString(", ")
          val msg = s"Unknown input '$x'. Expected one of: $available"
          ConfError.message(msg)
        }
    })
    val encoder = ConfEncoder.instance[T] { value =>
      options.collectFirst { case sourcecode.Text(`value`, source) =>
        Conf.Str(source)
      }.getOrElse(Conf.Null())
    }
    new ConfCodecEx(encoder, decoder)
  }

}
