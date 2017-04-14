package org.scalafmt

import scala.meta.Tree
import scala.meta.parsers.Parse

package object config extends ScalafmtConfDecoders {
  type MetaParser = Parse[_ <: Tree]
}
