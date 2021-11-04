package org.scalafmt

private[scalafmt] object CompatCollections {
  val JavaConverters = scala.jdk.CollectionConverters
  val ParConverters = scala.collection.parallel.CollectionConverters
}
