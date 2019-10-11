package org.scalafmt.cli

private[cli] object CompatParCollections {
  val Converters = {
    import Compat._
    {
      import scala.collection.parallel._
      CollectionConverters
    }
  }

  object Compat {
    object CollectionConverters
  }
}
