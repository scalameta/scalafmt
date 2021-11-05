package org.scalafmt

import java.util.HashMap

object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters

  // Scala native doesn't support concurrency
  object CompatParConverters {
    implicit class XIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col
    }
  }

  class ConcurrentMap[K, V] extends HashMap[K, V] {
    def contains(key: K): Boolean = {
      super.containsKey(key)
    }
  }
  def concurrentMap[K, V] = new ConcurrentMap[K, V]

}
