package org.scalafmt.config

import scala.reflect.ClassTag

import metaconfig._
import metaconfig.Configured._

object ReaderUtil {
  // Poor mans coproduct reader
  def oneOf[T: ClassTag](options: sourcecode.Text[T]*): ConfDecoder[T] = {
    val m = options.map(x => x.source.toLowerCase() -> x.value).toMap
    ConfDecoder.instance[T] {
      case Conf.Str(x) =>
        m.get(x.toLowerCase()) match {
          case Some(y) =>
            Ok(y)
          case None =>
            val available = options.map(_.source).mkString(", ")
            val msg = s"Unknown input '$x'. Expected one of $available"
            ConfError.msg(msg).notOk
        }
    }
  }
}
