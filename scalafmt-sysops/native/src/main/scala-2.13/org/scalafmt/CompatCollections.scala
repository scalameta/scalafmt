package org.scalafmt

object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters

  // Parallel collections are not released yet for Scala Native:
  // https://github.com/scala/scala-parallel-collections/issues/262
  // Once that releases, this should be able to be removed
  object ParConverters {
    implicit class XtensionIterable[T](val col: Iterable[T]) extends AnyVal {
      def compatPar = col
    }
  }
}
