package org.scalafmt

import scala.collection.parallel.CollectionConverters._

private[scalafmt] object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters
  object ParConverters {
    implicit class XtensionIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col.par
    }
  }
}
