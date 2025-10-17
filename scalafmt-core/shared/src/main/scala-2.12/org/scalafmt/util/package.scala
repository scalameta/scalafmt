package org.scalafmt

package object util {

  implicit class ImplicitIterator[A](private val obj: Iterator[A])
      extends AnyVal {
    def nextOption(): Option[A] = if (obj.hasNext) Some(obj.next()) else None
  }

}
