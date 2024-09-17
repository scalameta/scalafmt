package org.scalafmt

import java.util.HashMap

object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters

  // Scala Native does not include ParallelConverters
  object ParConverters {
    implicit class XIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col
    }
  }

  class ConcurrentMap[K, V] extends HashMap[K, V] {
    def contains(key: K): Boolean = super.containsKey(key)
  }
  def concurrentMap[K, V] = new ConcurrentMap[K, V]

}
