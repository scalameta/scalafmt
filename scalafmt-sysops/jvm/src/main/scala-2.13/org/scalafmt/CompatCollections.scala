package org.scalafmt

import java.util.concurrent._

import scala.collection.parallel.CollectionConverters._

private[scalafmt] object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters
  object ParConverters {
    implicit class XIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col.par
    }
  }
  def concurrentMap[K, V] = new ConcurrentHashMap[K, V]
}
