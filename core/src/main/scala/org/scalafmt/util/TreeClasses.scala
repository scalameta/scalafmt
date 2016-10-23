package org.scalafmt.util

import scala.meta.internal.classifiers.classifier
import scala.meta._

@classifier
trait SomeInterpolate
object SomeInterpolate {
  def unapply(tree: Tree): Boolean = {
    tree.is[Term.Interpolate] || tree.is[Pat.Interpolate]
  }
}
