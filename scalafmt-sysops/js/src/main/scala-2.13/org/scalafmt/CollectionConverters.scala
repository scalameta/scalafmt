package scala.collection.parallel

object CollectionConverters {
  implicit class XtensionIterable[T](val col: Iterable[T]) extends AnyVal {
    def par = col
  }
}
