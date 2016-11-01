package metaconfig

import scala.reflect.ClassTag

object Metaconfig {
  def getKey(map: Map[String, Any], keys: Seq[String]): Option[Any] =
    if (keys.isEmpty) None
    else map.get(keys.head).orElse(getKey(map, keys.tail))

  def get[T](map: Map[String, Any], className: String)(
      default: T,
      path: String,
      extraNames: String*)(implicit ev: Reader[T], clazz: ClassTag[T]) = {
    val value = getKey(map, path +: extraNames).getOrElse(default)
    ev.read(value) match {
      case Right(e) => e
      case Left(e: java.lang.IllegalArgumentException) =>
        val simpleName = clazz.runtimeClass.getSimpleName
        val msg =
          s"Error reading field '$path' on class $className. " +
            s"Expected argument of type $simpleName. " +
            s"Obtained ${e.getMessage}"
        throw _root_.metaconfig.ConfigError(msg)
      case Left(e) => throw e
    }
  }
}
