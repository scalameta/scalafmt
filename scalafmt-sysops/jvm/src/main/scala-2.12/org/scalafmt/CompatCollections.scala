package org.scalafmt

import java.util.concurrent._

private[scalafmt] object CompatCollections {
  val JavaConverters = scala.collection.JavaConverters
  object ParConverters {
    implicit class XIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col.par
    }
  }
  def concurrentMap[K, V] = new ConcurrentHashMap[K, V]
}
