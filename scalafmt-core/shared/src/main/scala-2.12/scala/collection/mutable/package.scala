package scala.collection

package object mutable {

  implicit class ImplicitMap[K, V](private val obj: Map[K, V]) extends AnyVal {
    def updateWith(
        key: K,
    )(remappingFunction: Option[V] => Option[V]): Option[V] = obj.get(key) match {
      case vOldOpt @ Some(vOld) =>
        val vOpt = remappingFunction(vOldOpt)
        vOpt match {
          case Some(v) => if (v != vOld) obj.update(key, v)
          case None => obj.remove(key)
        }
        vOpt
      case None =>
        val vOpt = remappingFunction(None)
        vOpt.foreach(v => obj.update(key, v))
        vOpt
    }
  }

}
