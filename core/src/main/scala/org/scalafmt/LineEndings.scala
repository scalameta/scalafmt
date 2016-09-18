package org.scalafmt

import scala.reflect.ClassTag

import metaconfig.Reader
import org.scalafmt.util.LoggerOps

sealed abstract class Docstrings
object Docstrings {
  val reader = ReaderUtil.oneOf[Docstrings](JavaDoc, ScalaDoc, preserve)
  case object JavaDoc extends Docstrings
  case object ScalaDoc extends Docstrings
  case object preserve extends Docstrings
}

sealed abstract class LineEndings

object LineEndings {
  val reader = ReaderUtil.oneOf[LineEndings](unix, windows, preserve)
  case object unix extends LineEndings
  case object windows extends LineEndings
  case object preserve extends LineEndings
}

object ReaderUtil {
  // Poor mans coproduct reader
  def oneOf[To: ClassTag](options: sourcecode.Text[To]*): Reader[To] = {
    val m = options.map(x => x.source -> x.value).toMap
    Reader.instance[To] {
      case x: String =>
        m.get(x) match {
          case Some(y) => Right(y)
          case None =>
            val available = m.keys.mkString(", ")
            val msg =
              s"Unknown input '$x'. Expected one of $available"
            Left(new IllegalArgumentException(msg))
        }
    }
  }
}
