package org.scalafmt

import scala.meta.Tree
import scala.meta.parsers.Parse

package object config {
  type MetaParser = Parse[_ <: Tree]

}
