package org.scalafmt

import scala.meta.Importee
import scala.util.Sorting

package object rewrite {

  implicit val importeeOrdering: Ordering[Importee] =
    new Ordering[Importee] {
      def compare(a: Importee, b: Importee) =
        a.tokens.mkString compare b.tokens.mkString
    }
}
