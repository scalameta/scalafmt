package org.scalafmt.util

import scala.meta.Case

import org.scalafmt.Error
import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.Tree
import scala.meta.Ctor
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Enumerator
import scala.meta.Mod
import scala.meta.Pat
import scala.meta.Pkg
import scala.meta.Source
import scala.meta.Template
import scala.meta.Term
import scala.meta.Type
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens
import scala.reflect.ClassTag
import scala.reflect.classTag

import org.scalafmt.Error.UnexpectedTree
import org.scalafmt.internal.FormatToken

/**
  * Stateless helper functions on [[scala.meta.Tree]].
  */
object TreeOps {
  import TokenOps._
  import LoggerOps._

  def getEnumStatements(enums: Seq[Enumerator]): Seq[Enumerator] = {
    val ret = Seq.newBuilder[Enumerator]
    enums.zipWithIndex.foreach {
      case (x, 0) => x
      case (enum: Enumerator.Guard, i) =>
        // Only guard that follows another guards starts a statement.
        if (enums(i - 1).is[Enumerator.Guard]) {
          ret += enum
        }
      case (x, _) => ret += x
    }
    ret.result()
  }

  def extractStatementsIfAny(tree: Tree): Seq[Tree] = tree match {
    case b: Term.Block => b.stats
    case t: Pkg => t.stats
    // TODO(olafur) would be nice to have an abstract "For" superclass.
    case t: Term.For => getEnumStatements(t.enums)
    case t: Term.ForYield => getEnumStatements(t.enums)
    case t: Term.Match => t.cases
    case t: Term.PartialFunction => t.cases
    case t: Term.TryWithCases => t.catchp
    case t: Type.Compound => t.refinement
    case t: scala.meta.Source => t.stats
    case t: Template if t.stats.isDefined => t.stats.get
    case t: Case if t.body.tokens.nonEmpty => Seq(t.body)
    case _ => Seq.empty[Tree]
  }

  def getDequeueSpots(tree: Tree): Set[TokenHash] = {
    val ret = Set.newBuilder[TokenHash]
    tree.tokens.foreach {
      case t @ KwElse() =>
        ret += hash(t)
      case _ =>
    }
    ret.result()
  }

  def getStatementStarts(tree: Tree): Map[TokenHash, Tree] = {
    val ret = Map.newBuilder[TokenHash, Tree]
    ret.sizeHint(tree.tokens.length)

    def addAll(trees: Seq[Tree]): Unit = {
      trees.foreach { t =>
        ret += hash(t.tokens.head) -> t
      }
    }

    def addDefn[T: ClassTag](mods: Seq[Mod], tree: Tree): Unit = {
      // Each @annotation gets a separate line
      val annotations = mods.filter(_.is[Mod.Annot])
      addAll(annotations)
      val firstNonAnnotation: Token = mods.collectFirst {
        case x if !x.is[Mod.Annot] =>
          // Non-annotation modifier, for example `sealed`/`abstract`
          x.tokens.head
      }.getOrElse {
        // No non-annotation modifier exists, fallback to keyword like `object`
        tree.tokens.find(x => classTag[T].runtimeClass.isInstance(x)) match {
          case Some(x) => x
          case None => throw Error.CantFindDefnToken[T](tree)
        }
      }
      ret += hash(firstNonAnnotation) -> tree
    }

    def loop(x: Tree): Unit = {
      x match {
        case t: Defn.Class => addDefn[KwClass](t.mods, t)
        case t: Defn.Def => addDefn[KwDef](t.mods, t)
        case t: Decl.Def => addDefn[KwDef](t.mods, t)
        case t: Ctor.Secondary => addDefn[KwDef](t.mods, t)
        case t: Defn.Object => addDefn[KwObject](t.mods, t)
        case t: Defn.Trait => addDefn[KwTrait](t.mods, t)
        case t: Defn.Type => addDefn[KwType](t.mods, t)
        case t: Decl.Type => addDefn[KwType](t.mods, t)
        case t: Defn.Val => addDefn[KwVal](t.mods, t)
        case t: Decl.Val => addDefn[KwVal](t.mods, t)
        case t: Defn.Var => addDefn[KwVar](t.mods, t)
        case t: Decl.Var => addDefn[KwVar](t.mods, t)
        case t => // Nothing
          addAll(extractStatementsIfAny(t))
      }
      x.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }

  /**
    * Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses(tokens: Tokens): Map[TokenHash, Token] = {
    val ret = Map.newBuilder[TokenHash, Token]
    var stack = List.empty[Token]
    tokens.foreach {
      case open @ (LeftBrace() | LeftBracket() | LeftParen() |
          Interpolation.Start()) =>
        stack = open :: stack
      case close @ (RightBrace() | RightBracket() | RightParen() |
          Interpolation.End()) =>
        val open = stack.head
        assertValidParens(open, close)
        ret += hash(open) -> close
        ret += hash(close) -> open
        stack = stack.tail
      case _ =>
    }
    val result = ret.result()
    result
  }

  def assertValidParens(open: Token, close: Token): Unit = {
    (open, close) match {
      case (Interpolation.Start(), Interpolation.End()) =>
      case (LeftBrace(), RightBrace()) =>
      case (LeftBracket(), RightBracket()) =>
      case (LeftParen(), RightParen()) =>
      case (o, c) =>
        throw new IllegalArgumentException(s"Mismatching parens ($o, $c)")
    }
  }

  /**
    * Creates lookup table from token offset to its closest scala.meta tree.
    */
  def getOwners(tree: Tree): Map[TokenHash, Tree] = {
    val result = Map.newBuilder[TokenHash, Tree]
    def loop(x: Tree): Unit = {
      x.tokens.foreach { tok =>
        result += hash(tok) -> x
      }
      x.children.foreach(loop)
    }
    loop(tree)
    result.result()
  }

  @tailrec
  final def childOf(child: Tree, tree: Tree): Boolean = {
    child == tree || (child.parent match {
      case Some(parent) => childOf(parent, tree)
      case _ => false
    })
  }

  def childOf(tok: Token, tree: Tree, owners: Map[TokenHash, Tree]): Boolean =
    childOf(owners(hash(tok)), tree)

  @tailrec
  final def parents(tree: Tree,
                    accum: Seq[Tree] = Seq.empty[Tree]): Seq[Tree] = {
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
    case d: Defn.Val => d.decltpe
    case d: Defn.Var => d.decltpe
    case _ => None
  }

  def isDefnSite(tree: Tree): Boolean = tree match {
    case _: Decl.Def | _: Defn.Def | _: Defn.Macro | _: Defn.Class |
        _: Defn.Trait | _: Ctor.Secondary | _: Decl.Type | _: Defn.Type |
        _: Type.Apply | _: Type.Param | _: Type.Tuple =>
      true
    case x: Ctor.Primary if x.parent.exists(_.isInstanceOf[Defn.Class]) =>
      true
    case _ => false
  }

  def isBinPackDefnSite(tree: Tree): Boolean = tree match {
    case _: Decl.Def | _: Defn.Def | _: Defn.Macro | _: Defn.Class |
        _: Defn.Trait | _: Ctor.Secondary | _: Defn.Type | _: Type.Apply |
        _: Type.Param | _: Type.Tuple =>
      true
    case x: Ctor.Primary if x.parent.exists(_.is[Defn.Class]) =>
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
    open.is[LeftParen] &&
    !isTuple(owner) &&
    owner.tokens.headOption.contains(open)
  }

  def isFirstOrLastToken(token: Token, owner: Tree): Boolean = {
    owner.tokens.headOption.contains(token) ||
    owner.tokens.lastOption.contains(token)
  }

  def isCallSite(tree: Tree): Boolean = tree match {
    case _: Term.Apply | _: Pat.Type.Apply | _: Pat.Extract | _: Term.Super |
        _: Pat.Tuple | _: Term.Tuple | _: Term.ApplyType | _: Term.Update =>
      true
    case _ => false
  }

  def isTuple(tree: Tree): Boolean = tree match {
    case _: Pat.Tuple | _: Term.Tuple | _: Type.Tuple => true
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
    case t: Term.Super => t -> Seq(t.superp)
    case t: Pat.Extract => t.ref -> t.args
    case t: Pat.Tuple => t -> t.elements
    case t: Pat.Type.Apply => t.tpe -> t.args
    case t: Term.ApplyType => t.fun -> t.targs
    case t: Term.Update => t.fun -> t.argss.flatten
    case t: Term.Tuple => t -> t.elements
    case t: Term.Function => t -> t.params
    case t: Type.Tuple => t -> t.elements
    case t: Type.Apply => t.tpe -> t.args
    case t: Type.Param => t.name -> t.tparams
    case t: Decl.Type => t.name -> t.tparams
    case t: Defn.Type => t.name -> t.tparams
    // TODO(olafur) flatten correct? Filter by this () section?
    case t: Defn.Def => t.name -> t.paramss.flatten
    case t: Defn.Macro => t.name -> t.paramss.flatten
    case t: Decl.Def => t.name -> t.paramss.flatten
    case t: Defn.Class => t.name -> t.ctor.paramss.flatten
    case t: Defn.Trait => t.name -> t.ctor.paramss.flatten
    case t: Ctor.Primary => t.name -> t.paramss.flatten
    case t: Ctor.Secondary => t.name -> t.paramss.flatten
  }

  val splitApplyIntoLhsAndArgsLifted = splitApplyIntoLhsAndArgs.lift

  def getApplyArgs(formatToken: FormatToken,
                   leftOwner: Tree): (Tree, Seq[Tree]) = {
    leftOwner match {
      case t: Defn.Def if formatToken.left.is[LeftBracket] =>
        t.name -> t.tparams
      // TODO(olafur) missing Defn.Def with `(` case.
      case _ =>
        splitApplyIntoLhsAndArgsLifted(leftOwner).getOrElse {
          logger.error(s"""Unknown tree
                           |${log(leftOwner.parent.get)}
                           |${isDefnSite(leftOwner)}""".stripMargin)
          throw UnexpectedTree[Term.Apply](leftOwner)
        }
    }
  }

  final def getSelectChain(select: Term.Select): Vector[Term.Select] = {
    select +: getSelectChain(select, Vector.empty[Term.Select])
  }

  @tailrec
  final def getSelectChain(child: Tree,
                           accum: Vector[Term.Select]): Vector[Term.Select] = {
    child.parent match {
      case Some(parent: Term.Select) =>
        getSelectChain(parent, accum :+ parent)
      case Some(parent)
          if splitApplyIntoLhsAndArgsLifted(parent).exists(_._1 == child) =>
        getSelectChain(parent, accum)
      case els => accum
    }
  }

  def startsSelectChain(tree: Tree): Boolean = tree match {
    case select: Term.Select =>
      !(existsChild(_.is[Term.Select])(select) &&
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
      case parent @ (_: Term.Apply | _: Term.ApplyInfix | _: Type.Apply) =>
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
      parent <- generator.parent if parent.is[Term.For] ||
        parent.is[Term.ForYield]
      sibling <- {
        val enums = parent match {
          case p: Term.For => p.enums
          case p: Term.ForYield => p.enums
        }
        val i = enums.indexOf(generator)
        if (i == -1)
          throw new IllegalStateException(
            s"Generator $generator is part of parents enums.")
        enums
          .drop(i + 1)
          .takeWhile(_.is[Enumerator.Guard])
          .lastOption
          .asInstanceOf[Option[Enumerator.Guard]]
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

  def defBody(tree: Tree): Option[Tree] = tree match {
    case t: Defn.Def => Some(t.body)
    case t: Defn.Macro => Some(t.body)
    case t: Ctor.Secondary => Some(t.body)
    case _ => None
  }

  @tailrec
  final def lastLambda(first: Term.Function): Term.Function =
    first.body match {
      case child: Term.Function => lastLambda(child)
      case block: Term.Block
          if block.stats.headOption.exists(_.is[Term.Function]) =>
        lastLambda(block.stats.head.asInstanceOf[Term.Function])
      case _ => first
    }

  final def isApplyInfix(op: Token.Ident, owner: Tree): Boolean =
    owner.parent.exists {
      case infix: Term.ApplyInfix => infix.op == owner
      case _ => false
    }

  @tailrec
  final def isTopLevelInfixApplication(child: Term.ApplyInfix): Boolean =
    child.parent match {
      case Some(parent: Term.ApplyInfix) => isTopLevelInfixApplication(parent)
      case Some(_: Term.Block | _: Term.If | _: Term.While | _: Source) => true
      case _ => false
    }
}
