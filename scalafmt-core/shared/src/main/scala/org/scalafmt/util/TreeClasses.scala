package org.scalafmt.util

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
@classifier
trait CaseClass
object CaseClass {
  def unapply(tree: Tree): Boolean =
    tree match {
      case defn: Defn =>
        defn.is[Defn.Class] && tree.children.exists(_.is[Mod.Case])
      case _ =>
        tree.parent.exists(_.is[CaseClass])
    }
}