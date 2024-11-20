package org.scalafmt

object CompatCollections {
  val JavaConverters = scala.collection.JavaConverters

  object ParConverters {
    implicit class XtensionIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col.par
    }
  }
}
