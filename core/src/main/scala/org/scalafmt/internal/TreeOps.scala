package org.scalafmt.internal

import org.scalafmt.internal.ScalaFmtLogger._
import scala.annotation.tailrec
import scala.meta.Tree

import scala.meta.internal.ast.Ctor
import scala.meta.internal.ast.Decl
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Enumerator
import scala.meta.internal.ast.Mod
import scala.meta.internal.ast.Pat
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Source
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Type
import scala.meta.tokens.Token._
import scala.meta.tokens.Token

/**
  * Stateless helper functions on [[scala.meta.Tree]].
  */
object TreeOps {
  import TokenOps._

  @tailrec
  final def childOf(child: Tree, tree: Tree): Boolean = {
    child == tree ||
    (child.parent match {
          case Some(parent) => childOf(parent, tree)
          case _ => false
        })
  }

  def childOf(tok: Token, tree: Tree, owners: Map[TokenHash, Tree]): Boolean =
    childOf(owners(hash(tok)), tree)

  @tailrec
  final def parents(
      tree: Tree, accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
    tree.parent match {
      case Some(parent) => parents(parent, parent +: accum)
      case _ => accum
    }
  }

  def isTopLevel(tree: Tree): Boolean = tree match {
    case _: Pkg | _: Source => true
    case _ => false
  }

  def isDefDef(tree: Tree): Boolean = tree match {
    case _: Decl.Def | _: Defn.Def => true
    case _ => false
  }

  def defDefReturnType(tree: Tree): Option[Type] = tree match {
    case d: Decl.Def => Some(d.decltpe)
    case d: Defn.Def => d.decltpe
    case _ => None
  }

  def isDefnSite(tree: Tree): Boolean = tree match {
    case _: Decl.Def | _: Defn.Def | _: Defn.Class | _: Defn.Trait |
        _: Ctor.Secondary | _: Type.Apply | _: Type.Param =>
      true
    case x: Ctor.Primary if x.parent.exists(_.isInstanceOf[Defn.Class]) =>
      true
    case _ => false
  }

  /**
    * Returns true if open is "unnecessary".
    *
    * An opening parenthesis is unnecessary if without it and its closing
    * parenthesis can be removed without changing the AST. For example:
    *
    * `(a(1))` will parse into the same tree as `a(1)`.
    */
  def isSuperfluousParenthesis(open: Token, owner: Tree): Boolean = {
    open.isInstanceOf[`(`] && !isTuple(owner) &&
    owner.tokens.headOption.contains(open)
  }

  def isFirstOrLastToken(token: Token, owner: Tree): Boolean = {
    owner.tokens.headOption.contains(token) ||
    owner.tokens.lastOption.contains(token)
  }

  def isCallSite(tree: Tree): Boolean = tree match {
    case _: Term.Apply | _: Pat.Extract | _: Pat.Tuple | _: Term.Tuple |
        _: Term.ApplyType | _: Term.Update =>
      true
    case _ => false
  }

  def isTuple(tree: Tree): Boolean = tree match {
    case _: Pat.Tuple | _: Term.Tuple => true
    case _ => false
  }

  def noSpaceBeforeOpeningParen(tree: Tree): Boolean =
    !isTuple(tree) && (isDefnSite(tree) || isCallSite(tree))

  def isModPrivateProtected(tree: Tree): Boolean = tree match {
    case _: Mod.Private | _: Mod.Protected => true
    case _ => false
  }

  def isTypeVariant(tree: Tree): Boolean = tree match {
    case _: Mod.Contravariant | _: Mod.Covariant => true
    case _ => false
  }

  val splitApplyIntoLhsAndArgs: PartialFunction[Tree, (Tree, Seq[Tree])] = {
    case t: Term.Apply => t.fun -> t.args
    case t: Pat.Extract => t.ref -> t.args
    case t: Pat.Tuple => t -> t.elements
    case t: Term.ApplyType => t.fun -> t.targs
    case t: Term.Update => t.fun -> t.argss.flatten
    case t: Term.Tuple => t -> t.elements
    case t: Type.Apply => t.tpe -> t.args
    case t: Type.Param => t.name -> t.tparams
    // TODO(olafur) flatten correct? Filter by this () section?
    case t: Defn.Def => t.name -> t.paramss.flatten
    case t: Decl.Def => t.name -> t.paramss.flatten
    case t: Defn.Class => t.name -> t.ctor.paramss.flatten
    case t: Defn.Trait => t.name -> t.ctor.paramss.flatten
    case t: Ctor.Primary => t.name -> t.paramss.flatten
    case t: Ctor.Secondary => t.name -> t.paramss.flatten
  }

  val splitApplyIntoLhsAndArgsLifted = splitApplyIntoLhsAndArgs.lift

  final def getSelectChain(select: Term.Select): Vector[Term.Select] = {
    select +: getSelectChain(select, Vector.empty[Term.Select])
  }

  @tailrec
  final def getSelectChain(
      child: Tree, accum: Vector[Term.Select]): Vector[Term.Select] = {
    child.parent match {
      case Some(parent: Term.Select) =>
        getSelectChain(parent, accum :+ parent)
      case Some(parent)
          if splitApplyIntoLhsAndArgsLifted(parent).exists(_._1 == child) =>
        getSelectChain(parent, accum)
      case els => accum
    }
  }

  def lastTokenInChain(chain: Vector[Term.Select]): Token = {
    if (chain.length == 1) lastToken(chain.last)
    else chainOptimalToken(chain)
  }

  def chainOptimalToken(chain: Vector[Term.Select]): Token = {
    chain.last.parent.map(splitApplyIntoLhsAndArgsLifted) match {
      case Some(_) => lastToken(chain.last.parent.get)
      case _ => lastToken(chain.last)
    }
  }

  def startsSelectChain(tree: Tree): Boolean = tree match {
    case select: Term.Select =>
      !(existsChild(_.isInstanceOf[Term.Select])(select) &&
          existsChild(splitApplyIntoLhsAndArgs.isDefinedAt)(select))
    case _ => false
  }

  /**
    * Returns true tree has a child for which f(child) is true.
    */
  def existsChild(f: Tree => Boolean)(tree: Tree): Boolean = {
    tree.children.exists(f) || tree.children.exists(existsChild(f))
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
        enums
          .zip(enums.tail)
          .collectFirst {
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

  @tailrec
  final def lastLambda(first: Term.Function): Term.Function =
    first.body match {
      case child: Term.Function => lastLambda(child)
      case block: Term.Block
          if block.stats.headOption.exists(_.isInstanceOf[Term.Function]) =>
        lastLambda(block.stats.head.asInstanceOf[Term.Function])
      case _ => first
    }
}
