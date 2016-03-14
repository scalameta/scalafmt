package org.scalafmt.internal

import scala.meta.Tree

import scala.meta.internal.ast.Ctor
import scala.meta.internal.ast.Decl
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Enumerator
import scala.meta.internal.ast.Mod
import scala.meta.internal.ast.Pat
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Source
import scala.meta.internal.ast.Template
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Type
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token._
import scala.meta.tokens.Token

/**
  * Stateless helper functions on [[scala.meta.Tree]].
  */
trait TreeOps extends TokenOps {

  def isTopLevel(tree: Tree): Boolean =
    tree match {
      case _: Pkg | _: Source => true
      case _ => false
    }

  def isDefDef(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def => true
      case _ => false
    }

  def defDefReturnType(tree: Tree): Option[Type] =
    tree match {
      case d: Decl.Def => Some(d.decltpe)
      case d: Defn.Def => d.decltpe
      case _ => None
    }

  def isDefnSite(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def | _: Defn.Class | _: Defn.Trait |
          _: Ctor.Secondary | _: Type.Apply | _: Type.Param =>
        true
      case x: Ctor.Primary if x.parent.exists(_.isInstanceOf[Defn.Class]) =>
        true
      case _ => false
    }

  def isCallSite(tree: Tree): Boolean =
    tree match {
      case _: Term.Apply | _: Pat.Extract | _: Pat.Tuple | _: Term.Tuple |
          _: Term.ApplyType | _: Term.Update =>
        true
      case _ => false
    }

  def noSpaceBeforeOpeningParen(tree: Tree): Boolean =
    isDefnSite(tree) || isCallSite(tree)

  def isModPrivateProtected(tree: Tree): Boolean =
    tree match {
      case _: Mod.Private | _: Mod.Protected => true
      case _ => false
    }

  def isTypeVariant(tree: Tree): Boolean =
    tree match {
      case _: Mod.Contravariant | _: Mod.Covariant => true
      case _ => false
    }

  /**
    * How many parents of tree are Term.Apply?
    */
  def nestedApplies(tree: Tree): Int = {
    // TODO(olafur) optimize?
    tree.parent.fold(0) {
      case parent@(_: Term.Apply | _: Term.ApplyInfix) =>
        1 + nestedApplies(parent)
      case parent => nestedApplies(parent)
    }
  }

  // TODO(olafur) abstract with [[NestedApplies]]

  def nestedSelect(tree: Tree): Int = {
    tree.parent.fold(0) {
      case parent: Term.Select => 1 + nestedSelect(parent)
      case parent => nestedSelect(parent)
    }
  }

  // TODO(olafur) scala.meta should make this easier.

  def findSiblingGuard(
      generator: Enumerator.Generator): Option[Enumerator.Guard] = {
    for {
      parent <- generator.parent if parent.isInstanceOf[Term.For] ||
               parent.isInstanceOf[Term.ForYield]
      sibling <- {
        val enums = parent match {
          case p: Term.For => p.enums
          case p: Term.ForYield => p.enums
        }
        enums.zip(enums.tail).collectFirst {
          case (`generator`, guard: Enumerator.Guard) => guard
        }
      }
    } yield sibling
  }

  /**
    * Calculates depth to deepest child in tree.
    */
  // TODO(olafur) inefficient, precalculate?

  def treeDepth(tree: Tree): Int =
    if (tree.children.isEmpty) 0
    else 1 + tree.children.map(treeDepth).max

  def templateCurly(owner: Tree): Token = {
    defnTemplate(owner).flatMap(templateCurly).getOrElse(owner.tokens.last)
  }

  def templateCurly(template: Template): Option[Token] = {
    template.tokens.find(_.isInstanceOf[`{`])
  }
}
