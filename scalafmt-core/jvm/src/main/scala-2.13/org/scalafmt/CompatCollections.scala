package org.scalafmt

import scala.collection.parallel.CollectionConverters._
import java.util.concurrent._

object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters
  object CompatParConverters {
    implicit class XIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col.par
    }
  }
  def concurrentMap[K, V] = new ConcurrentHashMap[K, V]

}
