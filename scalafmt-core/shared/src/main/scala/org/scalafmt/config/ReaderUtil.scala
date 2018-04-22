package org.scalafmt.config

import scala.reflect.ClassTag

import metaconfig._
import metaconfig.Configured._

object ReaderUtil {
  // Poor mans coproduct reader
  def oneOf[T: ClassTag](options: sourcecode.Text[T]*): ConfDecoder[T] =
    oneOfImpl(lowerCase, options)

  def oneOfIgnoreBackticks[T: ClassTag](
      options: sourcecode.Text[T]*): ConfDecoder[T] =
    oneOfImpl(lowerCaseNoBackticks, options)

  private def lowerCase(s: String): String = s.toLowerCase
  private def lowerCaseNoBackticks(s: String): String =
    s.toLowerCase().replace("`", "")

  private def oneOfImpl[T: ClassTag](
      sanitize: String => String,
      options: Seq[sourcecode.Text[T]]): ConfDecoder[T] = {
    val m = options.map(x => sanitize(x.source) -> x.value).toMap
    ConfDecoder.instance[T] {
      case Conf.Str(x) =>
        m.get(sanitize(x)) match {
          case Some(y) =>
            Ok(y)
          case None =>
            val available = m.keys.mkString(", ")
            val msg = s"Unknown input '$x'. Expected one of: $available"
            ConfError.message(msg).notOk
        }
    }
  }
}
