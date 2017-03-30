package org.scalafmt.util

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.internal.classifiers.classifier

@classifier
trait SomeInterpolate
object SomeInterpolate {
  def unapply(tree: Tree): Boolean = {
    tree.is[Term.Interpolate] || tree.is[Pat.Interpolate]
  }
}
@classifier
trait CtorModifier
object CtorModifier {
  def unapply(tree: Tree): Boolean =
    tree.is[Mod.Private] || tree.is[Mod.Protected]
}
