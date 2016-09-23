package metaconfig

object String2AnyMap {
  def unapply(arg: Any): Option[Map[String, Any]] = arg match {
    case someMap: Map[_, _] =>
      try {
        Some(someMap.asInstanceOf[Map[String, Any]])
      } catch {
        case _: ClassCastException =>
          None
      }
    case _ => None
  }
}
