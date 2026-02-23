package org.scalafmt.util

import org.scalafmt.Error
import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.LoggerOps._

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token.{Space => _, _}
import scala.meta.tokens.{Token => T, Tokens}

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable

/** Stateless helper functions on `scala.meta.Tree`.
  */
object TreeOps {
  import TokenOps._

  @tailrec
  def topTypeWith(typeWith: Type.With): Type.With = typeWith.parent match {
    case Some(t: Type.With) => topTypeWith(t)
    case _ => typeWith
  }

  @tailrec
  def withChain(top: Tree, res: Seq[Type.With] = Seq.empty): Seq[Type.With] =
    top match {
      case t: Type.With => withChain(t.lhs, t +: res)
      case _ => res
    }

  object SingleArgInBraces {
    def unapply(
        tree: Tree,
    )(implicit ftoks: FormatTokens): Option[(FT, Term, FT)] = tree match {
      case t: Term.ArgClause => unapply(t)
      case _ => None
    }
    def unapply(tree: Term.ArgClause)(implicit
        ftoks: FormatTokens,
    ): Option[(FT, Term, FT)] = getBraces(tree, tree.values)

    @inline
    private def getBraces[A <: Tree](tree: Tree, values: List[A])(implicit
        ftoks: FormatTokens,
    ): Option[(FT, A, FT)] = values match {
      case arg :: Nil => getBracesNested(tree, values).map { case (b, e) =>
          (b, arg, e)
        }
      case _ => None
    }

    @inline
    private def getBracesNested(tree: Tree, values: List[Tree])(implicit
        ftoks: FormatTokens,
    ): Option[(FT, FT)] = values match {
      case _ :: Nil => ftoks.getBracesIfEnclosed(tree) match {
          case None => tree.parent match {
              case Some(p: Term.ArgClause) => getBracesNested(p, p.values)
              case Some(p: Term.Block) => getBracesNested(p, p.stats)
              case _ => None
            }
          case x => x
        }
      case _ => None
    }

    def orBlock(
        tree: Tree,
    )(implicit ftoks: FormatTokens): Option[(FT, Stat, FT)] = tree match {
      case t: Term.ArgClause => unapply(t)
      case t: Term.Block => getBraces(t, t.stats)
      case _ => None
    }

    object OrBlock {
      def unapply(tree: Tree)(implicit
          ftoks: FormatTokens,
      ): Option[(FT, Stat, FT)] = orBlock(tree)
    }
  }

  @tailrec
  def isBlockFunction(fun: Tree)(implicit ftoks: FormatTokens): Boolean =
    fun.parent match {
      case Some(p: Term.FunctionLike) => isBlockFunction(p)
      case Some(p @ Term.Block(`fun` :: Nil)) => ftoks.getHead(p).left
          .is[T.LeftBrace] || isBlockFunction(p)
      case Some(SingleArgInBraces(_, `fun`, _)) => true
      case _ => false
    }

  def isFunctionWithBraces(fun: Member.Function)(implicit
      ftoks: FormatTokens,
  ): Boolean = fun.parent.exists(isExprWithParentInBraces(fun))

  def isExprWithParentInBraces(expr: Tree)(parent: Tree)(implicit
      ftoks: FormatTokens,
  ): Boolean = SingleArgInBraces.orBlock(parent).exists(_._2 eq expr)

  /** Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses[K, V](
      coll: Iterable[V],
  )(key: V => K)(f: V => T): Map[K, V] = {
    val ret = Map.newBuilder[K, V]
    var stack = List.empty[(T, V)]
    coll.foreach { elem =>
      f(elem) match {
        case open @ (_: T.OpenDelim | _: T.Xml.Start | _: T.Xml.SpliceStart |
            _: T.Interpolation.Start | _: T.Interpolation.SpliceStart) =>
          stack = (open, elem) :: stack
        case close @ (_: T.CloseDelim | _: T.Xml.End | _: T.Xml.SpliceEnd |
            _: T.Interpolation.End | _: T.Interpolation.SpliceEnd) =>
          val (open, openElem) = stack.head
          require(
            checkValidDelims(open, close),
            s"Mismatched delims ($open, $close)",
          )
          ret += key(openElem) -> elem
          ret += key(elem) -> openElem
          stack = stack.tail
        case _ =>
      }
    }
    if (stack.nonEmpty) throw new IllegalArgumentException(
      stack.map { case (x, _) => s"[${x.end}]$x" }
        .mkString("Orphan parens (", ", ", ")"),
    )
    val result = ret.result()
    result
  }

  def checkValidDelims(open: T, close: T): Boolean = open match {
    case _: T.Interpolation.Start => close.is[T.Interpolation.End]
    case _: T.Interpolation.SpliceStart => close.is[T.Interpolation.SpliceEnd]
    case _: T.Xml.Start => close.is[T.Xml.End]
    case _: T.Xml.SpliceStart => close.is[T.Xml.SpliceEnd]
    case _: T.LeftBrace => close.is[T.RightBrace]
    case _: T.LeftBracket => close.is[T.RightBracket]
    case _: T.LeftParen => close.is[T.RightParen]
    case _ => true
  }

  @tailrec
  final def numParents(tree: Tree, cnt: Int = 0)(f: Tree => Boolean): Int =
    tree.parent match {
      case Some(p) => numParents(p, if (f(p)) 1 + cnt else cnt)(f)
      case _ => cnt
    }

  /** Returns first ancestor which matches the given predicate.
    */
  def findTreeOrParent(
      tree: Tree,
  )(pred: Tree => Option[Boolean]): Option[Tree] = findTreeEx(tree)(t =>
    pred(t) match {
      case None => t.parent
      case Some(true) => Some(null)
      case Some(false) => None
    },
  )

  /** Returns first tree which matches the given predicate. The predicate
    * returns None to indicate failure; or the tree to recurse to; if the tree
    * is null (or the same as current tree), the current tree is returned.
    */
  @tailrec
  def findTreeEx(tree: Tree)(pred: Tree => Option[Tree]): Option[Tree] =
    pred(tree) match {
      case None => None
      case Some(null | `tree`) => Some(tree)
      case Some(r) => findTreeEx(r)(pred)
    }

  def findTreeOrParentSimple(tree: Tree, flag: Boolean = true)(
      pred: Tree => Boolean,
  ): Option[Tree] =
    findTreeOrParent(tree)(x => if (pred(x) == flag) Some(true) else None)

  /** Returns first ancestor whose parent matches the given predicate. The
    * predicate returns None to continue with the parent, or the boolean match
    * flag, which terminates the search.
    */
  def findTreeWithParent(tree: Tree)(
      pred: Tree => Option[Boolean],
  ): Option[Tree] = findTreeWithParentEx(tree)(t =>
    pred(t) match {
      case None => Some(t)
      case Some(true) => Some(null)
      case Some(false) => None
    },
  )

  /** Returns first ancestor whose parent matches the given predicate. The
    * predicate returns None to indicate failure; or the tree to recurse to; if
    * the recurse-to tree is null, the current tree is returned.
    */
  @tailrec
  def findTreeWithParentEx(
      tree: Tree,
  )(pred: Tree => Option[Tree]): Option[Tree] = tree.parent match {
    case None => None
    case Some(p) => pred(p) match {
        case None => None
        case Some(null) => Some(tree)
        case Some(r) => findTreeWithParentEx(r)(pred)
      }
  }

  def findTreeWithParentSimple(tree: Tree, flag: Boolean = true)(
      pred: Tree => Boolean,
  ): Option[Tree] =
    findTreeWithParent(tree)(x => if (pred(x) == flag) Some(true) else None)

  /** Returns first ancestor with a parent of a given type.
    */
  def findTreeWithParentOfType[A <: Tree](tree: Tree)(implicit
      classifier: Classifier[Tree, A],
  ): Option[Tree] = findTreeWithParentSimple(tree)(classifier.apply)

  /** Returns true if a matching ancestor of a given type exists.
    */
  @inline
  def existsParentOfType[A <: Tree](tree: Tree)(implicit
      classifier: Classifier[Tree, A],
  ): Boolean = findTreeWithParentOfType[A](tree).isDefined

  @tailrec
  def defDefBody(tree: Tree): Option[Tree] = tree match {
    case d: Defn with Tree.WithBody => Some(d.body)
    case d: Defn with Stat.WithTemplate => Some(d.templ.body)
    case t: Ctor.Block => Some(t)
    case t: Ctor.Secondary => Some(t.body)
    case _: Ctor.Primary | _: Pat.Var | _: Term.Name => tree.parent match {
        case Some(p) => defDefBody(p)
        case _ => None
      }
    case _ => None
  }
  @tailrec
  def defDefBodyParent(tree: Tree): Option[Tree] = tree.parent match {
    case Some(p: Member.ParamClauseGroup) => defDefBodyParent(p)
    case Some(p) => defDefBody(p)
    case None => None
  }

  // invoked ONLY for colon
  def colonDeclType(tree: Tree): Option[Type] = tree match {
    case d: Defn.Given => d.templ.inits.headOption.map(_.tpe)
    case d: Tree.WithDeclTpe => Some(d.decltpe) // all are Decl or Defn
    case d: Tree.WithDeclTpeOpt with Defn => d.decltpe
    case _ => None
  }

  object ColonDeclType {
    def unapply(tree: Tree): Option[Type] = colonDeclType(tree)
  }

  def isParamClauseSite(tree: Tree): Boolean = tree match {
    case _: Type.ParamClause => !tree.parent.is[Type.Lambda]
    case _: Term.ParamClause => tree.parent match {
        case Some(p: Term.FunctionTerm) => !isSeqSingle(p.paramClause.values)
        case _ => true
      }
    case _: Type.FuncParamClause | _: Type.Tuple => true
    case _: Member.Function => true // enclosed
    case _ => false
  }

  @inline
  def isTokenHeadOrBefore(token: T, owner: Tree): Boolean =
    isTokenHeadOrBefore(token, owner.pos)

  @inline
  def isTokenHeadOrBefore(token: T, pos: Position): Boolean = pos.start >=
    token.start

  @inline
  def isTokenLastOrAfter(token: T, owner: Tree): Boolean =
    isTokenLastOrAfter(token, owner.pos)

  @inline
  def isTokenLastOrAfter(token: T, pos: Position): Boolean = pos.end <=
    token.end

  def isArgClauseSite(tree: Tree): Boolean = tree match {
    case t: Member.ArgClause => !t.parent.is[Member.Infix] ||
      (t.values match {
        case (_: Term.Assign | _: Lit.Unit) :: Nil => true
        case Nil | _ :: Nil => false
        case _ => true
      })
    case _: Term.Super | _: Lit.Unit | _: Term.Tuple | _: Pat.Tuple => true
    case _ => false
  }

  def noSpaceBeforeOpeningParen(tree: Tree): Boolean = tree match {
    case _: Term.Super => true
    case t: Member.ArgClause => !t.parent.is[Member.Infix]
    case _: Member.ParamClause => tree.parent.exists {
        case _: Member.Function => false
        case t: Ctor.Primary => t.mods.isEmpty ||
          !t.paramClauses.headOption.contains(tree)
        case _ => true
      }
    case _ => false
  }

  /** How many parents of tree are Term.Apply?
    */
  @tailrec
  def nestedApplies(tree: Tree): Int = tree match {
    case _: Member.SyntaxValuesClause | _: Member.ParamClauseGroup =>
      tree.parent match {
        case Some(p) => nestedApplies(p)
        case _ => 0
      }
    case _ => numParents(tree) {
        case _: Term.Apply | _: Term.ApplyInfix | _: Type.Apply => true
        case _ => false
      }
  }

  def nestedSelect(tree: Tree): Int = numParents(tree)(_.is[Term.Select])

  /** Calculates depth to deepest child in tree.
    */
  // TODO(olafur) inefficient, precalculate?

  def treeDepth(tree: Tree): Int = tree match {
    case Member.ParamClauseGroup(tparams, paramss) =>
      maxTreeDepth(tparams +: paramss)
    case Member.SyntaxValuesClause(v) => v match {
        case (b: Tree.Block) :: Nil => maxTreeDepth(b.stats)
        case _ => maxTreeDepth(v)
      }
    case _ =>
      val children = tree.children
      if (children.isEmpty) 0 else 1 + maxTreeDepth(children)
  }

  def maxTreeDepth(trees: Seq[Tree]): Int = trees.foldLeft(0) { case (res, t) =>
    math.max(res, treeDepth(t))
  }

  def getSingleArgOnLeftBraceOnLeft(ft: FT)(implicit
      ftoks: FormatTokens,
  ): Option[(Term.ArgClause, Stat)] = ft.leftOwner match {
    case ac: Term.ArgClause => (ac.values match {
        case (t: Term.Block) :: Nil if ftoks.getHead(t) eq ft =>
          getBlockSingleStat(t)
        case t :: Nil => Some(t)
        case _ => None
      }).map(x => (ac, x))
    case t: Term => t.parent match {
        case Some(ac: Term.ArgClause) if ac.values.lengthCompare(1) == 0 =>
          (t match {
            case t: Term.Block if ftoks.getHead(ac) eq ft =>
              getBlockSingleStat(t)
            case _ => Some(t)
          }).map(x => (ac, x))
        case _ => None
      }
    case _ => None
  }

  def getSingleArgLambdaPenalties(
      ac: Term.ArgClause,
      arg: Stat,
  ): Option[(Int, Int)] =
    if (arg.is[Term.FunctionLike]) Some((nestedApplies(ac), 2))
    else ac.parent match {
      case Some(p: Term.Apply) => Some((nestedApplies(p), treeDepth(p.fun)))
      case _ => None
    }

  def getLambdaPenaltiesOnLeftBraceOnLeft(ft: FormatToken)(implicit
      ftoks: FormatTokens,
  ): Option[(Int, Int)] = getSingleArgOnLeftBraceOnLeft(ft)
    .flatMap((getSingleArgLambdaPenalties _).tupled)

  final def canBreakAfterFuncArrow(
      func: Member.Function,
  )(implicit ftoks: FormatTokens, style: ScalafmtConfig): Boolean =
    func.paramClause match {
      case params: Term.ParamClause
          if style.dialect.allowFewerBraces && params.mod.isEmpty =>
        params.values match {
          case (param: Term.Param) :: Nil => param.decltpe match {
              case Some(_: Type.Name) => ftoks.isEnclosedInMatching(params)
              case _ => true
            }
          case _ => true
        }
      case _ => true
    }

  @tailrec
  final def lastLambda(
      first: Member.Function,
      res: Option[Member.Function] = None,
  )(implicit
      ftoks: FormatTokens,
      style: ScalafmtConfig,
  ): Option[Member.Function] = {
    val nextres = if (canBreakAfterFuncArrow(first)) Some(first) else res
    first.body match {
      case child: Member.Function => lastLambda(child, nextres)
      case b @ Term.Block((child: Member.Function) :: Nil)
          if !ftoks.getHead(b).left.is[T.LeftBrace] => lastLambda(child, nextres)
      case _ => nextres
    }
  }

  @inline
  final def isInfixOp(tree: Tree): Boolean = AsInfixOp.unapply(tree).isDefined

  object AsInfixOp {
    def apply(tree: Tree) = unapply(tree)
    def unapply(tree: Tree): Option[Member.Infix] = tree.parent
      .collect { case ia: Member.Infix if ia.op eq tree => ia }
  }

  @inline
  final def asInfixApp(tree: Tree): Option[Member.Infix] = InfixApp.unapply(tree)

  @inline
  final def isInfixApp(tree: Tree): Boolean = asInfixApp(tree).isDefined

  @tailrec
  def isInfixArg(tree: Tree): Boolean = tree.parent match {
    case None => false
    case Some(p) => p match {
        case _: Member.ArgClause => isInfixArg(p)
        case p: Member.Infix => (p.arg eq tree) || isInfixArg(p)
        case _ => false
      }
  }

  @tailrec
  def findNextInfixInParent(tree: Tree, scope: Tree)(implicit
      ftoks: FormatTokens,
  ): Option[Member.Infix] = tree.parent match {
    case Some(t: Member.ArgClause) => findNextInfixInParent(t, scope)
    case Some(t: Term.Block) if !ftoks.isEnclosedInBraces(t) =>
      findNextInfixInParent(t, scope)
    case Some(t: Member.Infix) if tree ne scope =>
      if (t.lhs eq tree) Some(t) else findNextInfixInParent(t, scope)
    case _ => None
  }

  def infixSequenceLength(app: Member.Infix): Int = {
    val queue = new mutable.Queue[Member.Infix]()
    queue += app
    var length = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      length += 1
      queue ++= elem.nestedInfixApps
    }
    length
  }

  // procedure syntax has decltpe: Some("")
  def isProcedureSyntaxDeclTpe(tpe: Type): Boolean = tpe.tokens.isEmpty

  def isProcedureSyntax(defn: Defn.Def): Boolean = defn.decltpe
    .exists(isProcedureSyntaxDeclTpe)

  def isXmlBrace(owner: Tree): Boolean = owner match {
    case _: Term.Xml | _: Pat.Xml => true
    case b: Term.Block => b.parent.is[Term.Xml]
    case _ => false
  }

  def getAssignAtSingleArgCallSite(args: Seq[Tree]): Option[Term.Assign] =
    args match {
      case Seq(fun: Term.Assign) => Some(fun)
      case _ => None
    }

  @inline
  def isSeqSingle(seq: Seq[_]): Boolean = seq.lengthCompare(1) == 0

  @inline
  def isSeqMulti(seq: Seq[_]): Boolean = seq.lengthCompare(1) > 0

  @inline
  def isSingleStatBlock(tree: Term.Block): Boolean = isSeqSingle(tree.stats)

  @inline
  def isMultiStatBlock(tree: Term.Block): Boolean = isSeqMulti(tree.stats)

  def getSingleElement[A](elements: List[A]): Option[A] = elements match {
    case elem :: Nil => Some(elem)
    case _ => None
  }

  @inline
  def getSingleElement(tree: Tree.Block): Option[Tree] =
    getSingleElement(tree.stats)

  @inline
  def hasSingleElement(tree: Tree.Block, value: Tree): Boolean =
    getSingleElement(tree).contains(value)

  @inline
  def hasSingleElement(tree: Member.SyntaxValuesClause, value: Tree): Boolean =
    getSingleElement(tree.values).contains(value)

  def getBlockSingleStat(b: Term.Block): Option[Stat] = b.stats match {
    case stat :: Nil => Some(stat)
    case _ => None
  }

  def isTreeMultiStatBlock(tree: Tree): Boolean = tree match {
    case t: Term.Block => isMultiStatBlock(t)
    case _ => false
  }

  @tailrec
  def getTreeSingleExpr(tree: Tree): Option[Term] = tree match {
    case t: Term.Block => t.stats match {
        case stat :: Nil => getTreeSingleExpr(stat)
        case _ => None
      }
    case t: Term => Some(t)
    case _ => None
  }

  def isTreeSingleExpr(tree: Tree): Boolean = getTreeSingleExpr(tree).isDefined

  /* An end marker is really more like a closing brace for formatting purposes
   * (but not when rewriting) so we should ignore it when considering whether a
   * block contains only a single statement. NB: in FormatWriter, when choosing
   * to insert or remove end markers, we avoid such borderline cases.
   */
  def getSingleStatExceptEndMarker[A <: Tree](ss: List[A]): Option[A] = ss match {
    case s :: rs if (rs match {
          case Nil | (_: Term.EndMarker) :: Nil => true
          case _ => false
        }) => Some(s)
    case _ => None
  }

  def getSingleStatExceptEndMarker(t: Tree): Option[Tree] = t match {
    case Term.Block(s) => getSingleStatExceptEndMarker(s)
    case _ => Some(t)
  }

  def getTreeSingleStat(t: Tree): Option[Tree] = t match {
    case b: Term.Block => getBlockSingleStat(b)
    case _ => Some(t)
  }

  def getTreeLineSpan(b: Tree): Int = getTreeLineSpan(b.pos)
  def getTreeLineSpan(pos: Position): Int =
    if (pos.isEmpty) 0 else pos.endLine - pos.startLine

  def hasSingleTermStat(t: Term.Block): Boolean = getBlockSingleStat(t).is[Term]

  def hasSingleTermStatIfBlock(t: Tree): Boolean = t match {
    case b: Term.Block => hasSingleTermStat(b)
    case _ => true
  }

  @tailrec
  def getBlockStat(t: Tree)(implicit ftoks: FormatTokens): Tree = t match {
    case b: Term.Block => getSingleStatExceptEndMarker(b.stats) match {
        case Some(s) if !ftoks.isEnclosedInBraces(b) => getBlockStat(s)
        case _ => t
      }
    case _ => t
  }

  /** In cases like:
    * {{{
    *   class X(
    *     implicit
    *     private[this] val i1: Int,
    *     private[this] var i2: String,
    *     implicit private[this] var i3: Boolean
    * )
    * }}}
    *
    * `val i1`, and `var i2` positions do not include their ``Mod.Implicit``.
    * `var i3` has a ``Mod.Implicit`` which is included.
    */
  def noExplicitImplicit(m: Mod.Implicit, ownerStart: Int): Boolean = {
    val modPos = m.pos
    modPos.start < ownerStart || modPos.isEmpty
  }

  def noExplicitImplicit(ownerStart: Int, orElse: Boolean)(m: Mod): Boolean =
    m match {
      case m: Mod.Implicit => noExplicitImplicit(m, ownerStart)
      case _ => orElse
    }

  def noExplicitImplicit(param: Term.Param): Boolean = {
    val pStart = param.pos.start
    param.mods.forall(noExplicitImplicit(pStart, true))
  }

  def getImplicitParamList(kwOwner: Tree): Option[Member.SyntaxValuesClause] =
    kwOwner.parent match {
      case Some(v @ Term.ArgClause(_, Some(`kwOwner`))) => Some(v)
      case Some(v @ Term.ParamClause(_ :: rest, Some(`kwOwner`)))
          if !kwOwner.is[Mod.Implicit] || rest.isEmpty ||
            rest.exists(noExplicitImplicit) => Some(v)
      case _ => None
    }

  def hasImplicitParamList(kwOwner: Tree): Boolean =
    getImplicitParamList(kwOwner).isDefined

  def getEndOfFirstCall(tree: Tree)(implicit ftoks: FormatTokens) = {
    @tailrec
    def traverse(tree: Tree, res: Option[Tree]): Option[Tree] = tree match {
      case t: Term.SelectLike if res.isDefined => traverse(t.qual, Some(t.qual))
      case t: Term.ApplyType => traverse(t.fun, Some(t))
      case t: Member.Apply => traverse(t.fun, Some(t.fun))
      case t: Init => traverse(t.tpe, Some(t.tpe))
      case Term.Block(arg :: Nil) if !ftoks.isEnclosedInBraces(tree) =>
        traverse(arg, res)
      case _ => res
    }
    traverse(tree, None).map(ftoks.getLast)
  }

  @inline
  def getLastCall(tree: Tree): Tree = getLastCall(tree, tree)

  @tailrec
  private def getLastCall(tree: Tree, lastCall: Tree): Tree = {
    // this is to cover types which include one parameter group at a time
    val matches = tree match {
      case t: Member.Apply if t.fun eq lastCall => true
      case _ => tree eq lastCall
    }
    if (matches) tree.parent match {
      case Some(p) => getLastCall(p, tree)
      case _ => tree
    }
    else lastCall
  }

  @tailrec
  def findInterpolate(tree: Tree): Option[Term.Interpolate] = tree match {
    case ti: Term.Interpolate => Some(ti)
    case _ => tree.parent match {
        case Some(p) => findInterpolate(p)
        case _ => None
      }
  }

  def findArgAfter(end: Int, trees: Seq[Tree]): Option[Tree] = trees
    .find(_.pos.start >= end)

  def getStripMarginCharForInterpolate(tree: Tree): Option[Char] =
    findInterpolate(tree).flatMap(getStripMarginChar)

  def getStripMarginChar(t: Tree): Option[Char] = t.parent match {
    case Some(ts: Term.SelectLike) if ts.name.value == "stripMargin" =>
      ts.parent match {
        case Some(Term.Apply.Initial(_, List(arg: Lit.Char))) => Some(arg.value)
        case _ => Some('|')
      }
    case _ => None
  }

  @inline
  def isTripleQuote(syntax: String): Boolean = syntax.startsWith("\"\"\"")

  @tailrec
  def findFirstTreeBetween(tree: Tree, beg: T, end: T): Option[Tree] = {
    def isWithinRange(x: Tokens): Boolean = x.nonEmpty &&
      x.head.start >= beg.start && x.last.end <= end.end
    def matches(tree: Tree): Boolean = {
      val x = tree.tokens
      isWithinRange(x) ||
      x.nonEmpty && x.head.start <= beg.start && x.last.end >= end.end
    }
    if (isWithinRange(tree.tokens)) Some(tree)
    else tree.children.find(matches) match {
      case Some(c) => findFirstTreeBetween(c, beg, end)
      case _ => None
    }
  }

  @inline
  def ifWithoutElse(t: Term.If) = t.elsep.is[Lit.Unit]

  def ifWithoutElse(tree: Tree): Boolean = tree match {
    case t: Term.If => ifWithoutElse(t)
    case _ => false
  }

  @tailrec
  def existsIfWithoutElse(t: Term.If): Boolean = existsIfWithoutElse(t.thenp) ||
    (t.elsep match {
      case x: Term.If => existsIfWithoutElse(x)
      case _ => ifWithoutElse(t)
    })

  def existsIfWithoutElse(tree: Tree): Boolean = tree match {
    case t: Term.If => existsIfWithoutElse(t)
    case _ => false
  }

  def cannotStartSelectChainOnExpr(expr: Term): Boolean = expr match {
    case _: Term.Placeholder => true
    case t: Term.Name => isSymbolicName(t.value)
    case t: Term.Select => isSymbolicName(t.name.value)
    case _ => false
  }

  // Redundant {} block around case statements
  def isCaseBodyABlock(ft: FT, caseStat: CaseTree): Boolean = ft.right
    .is[T.LeftBrace] && (caseStat.body eq ft.meta.rightOwner)

  def getTemplateGroups(template: Template): Option[Seq[List[Tree]]] = {
    val groups = Seq(template.inits, template.derives).filter(_.nonEmpty)
    if (groups.isEmpty) None else Some(groups)
  }

  // Scala syntax allows commas before right braces in weird places,
  // like constructor bodies:
  // def this() = {
  //   this(1),
  // }
  // This code simply ignores those commas because it does not
  // consider them "trailing" commas. It does not remove them
  // in the TrailingCommas.never branch, nor does it
  // try to add them in the TrainingCommas.always branch.
  def rightIsCloseDelimForTrailingComma(
      left: T,
      ft: FT,
      whenNL: Boolean = true,
  ): Boolean = {
    def owner = ft.meta.rightOwner
    def isArgOrParamClauseSite(tree: Tree) = !whenNL || isArgClauseSite(tree) ||
      isParamClauseSite(tree) &&
      (tree match { // exclude extended instance; type/`using` clause is ok
        case t: Term.ParamClause if t.mod.isEmpty && isSeqSingle(t.values) =>
          tree.parent.forall {
            case p: Member.ParamClauseGroup => !p.parent.is[Defn.ExtensionGroup]
            case _ => true
          }
        case _ => true
      })
    // skip empty parens/braces/brackets
    ft.right match {
      case _: T.RightBrace => !left.is[T.LeftBrace] && owner.is[Importer]
      case _: T.RightParen => !left.is[T.LeftParen] &&
        isArgOrParamClauseSite(owner)
      case _: T.RightBracket => !left.is[T.LeftBracket] &&
        isArgOrParamClauseSite(owner)
      case _ => false
    }
  }

  def findEnclosedBetweenParens(lt: T, rt: T, tree: Tree): Option[Tree] = {
    val beforeParens = lt.start
    val afterParens = rt.end
    @tailrec
    def iter(trees: List[Tree]): Option[Tree] = trees match {
      case head :: rest =>
        val headPos = head.pos
        val headEnd = headPos.end
        if (headEnd <= beforeParens) iter(rest)
        else if (
          headEnd <= afterParens && headPos.start < headEnd &&
          rest.headOption.forall(_.pos.start >= afterParens)
        ) Some(head)
        else None
      case _ => None
    }
    val pos = tree.pos
    val found = beforeParens <= pos.start && pos.end <= afterParens
    if (found) Some(tree) else iter(tree.children)
  }

  def getStyleAndOwners(
      topSourceTree: Tree,
      baseStyle: ScalafmtConfig,
  ): (ScalafmtConfig, collection.Map[TokenHash, Tree]) = {
    var termInfixCount = 0
    var typeInfixCount = 0
    var patInfixCount = 0
    // Creates lookup table from token offset to its closest scala.meta tree
    val ownersMap = HashMap.newBuilder[TokenHash, Tree]
    @inline
    def setOwner(tok: T, tree: Tree): Unit = ownersMap += hash(tok) -> tree

    val allTokens = topSourceTree.tokens
    var prevParens: List[T] = Nil

    def treeAt(
        elemIdx: Int,
        elem: Tree,
        elemBeg: T,
        elemEnd: T,
        outerPrevLPs: Int,
    ): Int = {
      elem match {
        case _: Term.ApplyInfix => termInfixCount += 1
        case _: Type.ApplyInfix => typeInfixCount += 1
        case _: Pat.ExtractInfix => patInfixCount += 1
        case _ =>
      }

      val treeBeg = elemBeg.start
      val treeEnd = elemEnd.end
      val allChildren: List[(Tree, T, T)] = {
        for {
          x <- elem.children
          tokens = x.tokens if tokens.nonEmpty
          beg = tokens.head if beg.start >= treeBeg // sometimes with implicit
          end = tokens.last if end.end <= treeEnd
        } yield (x, beg, end)
      }.sortBy(_._2.start)

      allChildren match {
        case Nil =>
          @tailrec
          def tokenAt(idx: Int): Int = {
            val tok = allTokens(idx)
            setOwner(tok, elem)
            val nextIdx = idx + 1
            if (tok eq elemEnd) nextIdx else tokenAt(nextIdx)
          }
          tokenAt(elemIdx)

        case (firstChild, firstChildBeg, firstChildEnd) :: rest =>
          var nextChild = firstChild
          var nextChildBeg = firstChildBeg
          var nextChildEnd = firstChildEnd
          var children = rest
          var prevChild: Tree = null
          var prevLPs = outerPrevLPs
          var prevComma: T = null

          @tailrec
          def tokenAt(idx: Int): Int =
            if (idx > 0 && (elemEnd eq allTokens(idx - 1))) idx
            else {
              val tok = allTokens(idx)
              if (tok eq nextChildBeg) {
                if (prevChild != null) prevLPs = 0
                prevChild = nextChild
                val nextIdx = treeAt(idx, nextChild, tok, nextChildEnd, prevLPs)
                children match {
                  case Nil => nextChildBeg = null
                  case (head, beg, end) :: rest =>
                    children = rest
                    nextChild = head
                    nextChildBeg = beg
                    nextChildEnd = end
                }
                prevComma = null
                tokenAt(nextIdx)
              } else {
                def excludeRightParen: Boolean = elem match {
                  case t: Term.If => prevLPs == 1 && prevChild == t.cond // `expr` after `mods`
                  case _: Term.While | _: Term.ForClause => prevLPs == 1 &&
                    prevChild == firstChild // `expr` is first
                  case _: Member.SyntaxValuesClause | _: Member.Tuple |
                      _: Term.Do | _: Term.AnonymousFunction => elemEnd eq tok
                  case t: Init => prevChild ne t.tpe // include tpe
                  case _: Ctor.Primary | _: Term.EnumeratorsBlock => true
                  case _ => false
                }

                if (prevParens.nonEmpty && tok.is[RightParen]) {
                  if (prevChild == null || prevLPs <= 0 || excludeRightParen)
                    setOwner(tok, elem)
                  else {
                    setOwner(tok, prevChild)
                    setOwner(prevParens.head, prevChild)
                    if (prevComma != null) setOwner(prevComma, prevChild)
                  }
                  prevLPs -= 1
                  prevParens = prevParens.tail
                  prevComma = null
                } else if (tok.is[Comma]) {
                  prevComma = tok
                  setOwner(tok, elem)
                } else {
                  setOwner(tok, elem)
                  if (!tok.is[Trivia] && !tok.isEmpty) {
                    prevComma = null
                    prevChild = null
                    if (tok.is[LeftParen]) {
                      prevLPs += 1
                      prevParens = tok :: prevParens
                    } else prevLPs = 0
                  }
                }
                tokenAt(idx + 1)
              }
            }
          tokenAt(elemIdx)
      }
    }

    treeAt(0, topSourceTree, allTokens.head, allTokens.last, 0)

    val checkedNewlines = baseStyle.newlines
      .checkInfixConfig(termInfixCount, typeInfixCount, patInfixCount)(baseStyle)
    val checkedRunner = addDialectFeatures(baseStyle.runner, topSourceTree)
    val ok = (checkedNewlines eq baseStyle.newlines) &&
      (checkedRunner eq baseStyle.runner)
    val initStyle =
      if (ok) baseStyle
      else baseStyle.copy(newlines = checkedNewlines, runner = checkedRunner)
    (initStyle, ownersMap.result())
  }

  def isFewerBraces(
      tree: Term.Apply,
  )(implicit dialect: Dialect, ftoks: FormatTokens): Boolean =
    dialect.allowFewerBraces && ftoks.getHead(tree.argClause).left.is[Colon]

  @tailrec
  def isFewerBracesLhs(tree: Tree)(implicit
      dialect: Dialect,
      ftoks: FormatTokens,
  ): Boolean = !ftoks.isEnclosedInMatching(tree) &&
    (tree match {
      case t: Term.Apply => isFewerBraces(t)
      case t: Term.ApplyInfix => isFewerBracesLhs(t.argClause)
      case Term.ArgClause(arg :: Nil, _) => isFewerBracesLhs(arg)
      case t: Term.AnonymousFunction => t.body match {
          case t: Term.Apply => isFewerBraces(t)
          case t: Term.ApplyInfix => isFewerBracesLhs(t.argClause)
          case _ => false
        }
      case t: Tree.WithCasesBlock => !ftoks.isEnclosedInMatching(t.casesBlock)
      case _ => false
    })

  @tailrec
  def isFewerBracesRhs(tree: Tree)(implicit
      dialect: Dialect,
      ftoks: FormatTokens,
  ): Boolean = !ftoks.isEnclosedInMatching(tree) &&
    (tree match {
      case t: Term.Apply => isFewerBraces(t)
      case t: Term.ApplyInfix => isFewerBracesRhs(t.lhs)
      case Term.ArgClause(arg :: Nil, _) => isFewerBracesRhs(arg)
      case t: Term.AnonymousFunction => t.body match {
          case t: Term.Apply => isFewerBraces(t)
          case t: Term.ApplyInfix => isFewerBracesRhs(t.lhs)
          case _ => false
        }
      case _ => false
    })

  def isParentAnApply(t: Tree): Boolean = t.parent.is[Term.Apply]

  def isCapturingBrace(owner: Tree): Boolean = owner match {
    case _: Type.Capturing | _: Type.Captures => true
    case t: Type.FunctionLikeType => t.parent.is[Type.Capturing]
    case _ => false
  }

  def braceSpace(owner: Tree): Modification = Space {
    def isXml(t: Tree) = t.isAny[Term.Xml, Pat.Xml]
    owner match {
      case t: Term.Block => !t.parent.exists(isXml)
      case t => !isXml(t) && !isCapturingBrace(t)
    }
  }

  def isEmptyTree(tree: Tree): Boolean = tree match {
    case t: Term.Block => t.stats.isEmpty
    case t => t.tokens.isEmpty
  }

  def isEmptyFunctionBody(tree: Tree): Boolean = tree match {
    case t: Member.Function => isEmptyTree(t.body)
    case _ => false
  }

  val getArgsPartial: PartialFunction[Tree, List[Tree]] = {
    case _: Lit.Unit => Nil
    case t: Term.Super => t.superp :: Nil
    case Member.Tuple(v) => v
    case Member.SyntaxValuesClause(v) => v
    case t: Member.Function => t.paramClause.values
  }

  def getArgsOrNil(owner: Tree): List[Tree] = getArgsPartial.lift(owner)
    .getOrElse(Nil)

  private def throwUnexpectedGetArgs(t: Tree): Nothing = {
    logger.debug(
      s"""|getArgs: unknown tree
          |Tree: ${log(t)}
          |Parent: ${log(t.parent)}
          |GrandParent: ${log(t.parent.flatMap(_.parent))}
          |""".stripMargin,
    )
    throw Error.UnexpectedTree[Member.SyntaxValuesClause](t)
  }

  def getArgs(owner: Tree): List[Tree] = getArgsPartial
    .applyOrElse(owner, throwUnexpectedGetArgs)

  @tailrec
  def couldHaveBracesConvertedToParens(tree: Tree): Boolean = tree match {
    case _: Tree.CasesBlock => false
    case t: Term.ArgClause => t.values match {
        case Nil => true
        case x :: Nil => couldHaveBracesConvertedToParens(x)
        case _ => false
      }
    case t: Term.Block => t.stats match {
        case Nil => true
        case x :: Nil => couldHaveBracesConvertedToParens(x)
        case _ => false
      }
    case _ => true
  }

  @tailrec
  def isTreeEndingInArgumentClause(tree: Tree): Boolean = tree match {
    case t: Init => t.argClauses.nonEmpty
    case t: Term.Apply => t.argClause.nonEmpty
    case t: Term.ApplyType => t.argClause.nonEmpty
    case t: Tree.WithCasesBlock => t.casesBlock.cases.nonEmpty
    case t: Term.New => t.init.argClauses.nonEmpty
    case _: Term.NewAnonymous => true
    case t: Term.AnonymousFunction => isTreeEndingInArgumentClause(t.body)
    case _ => false
  }

  @tailrec
  def getBlockWithNonSingleTermStat(t: Term.Block): Option[Term.Block] =
    t.stats match {
      case (x: Term.Block) :: Nil => getBlockWithNonSingleTermStat(x)
      case (_: Term) :: Nil => None
      case _ :: _ => Some(t)
      case _ => None
    }

  // look for arrow before body, if any, else after params
  def getFuncArrow(term: Member.Function)(implicit
      ftoks: FormatTokens,
  ): Option[FT] = ftoks.tokenBeforeOpt(term.body).orElse(
    ftoks.tokenAfterOpt(term.paramClause).map(getArrowAfter[T.FunctionArrow]),
  )

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: Case)(implicit ftoks: FormatTokens): FT = ftoks
    .tokenBeforeOpt(term.body).getOrElse(getArrowAfter[T.RightArrow](
      ftoks.tokenAfter(term.cond.getOrElse(term.pat)),
    ))

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: TypeCase)(implicit ftoks: FormatTokens): FT = ftoks
    .next(ftoks.tokenAfter(term.pat))

  private def getArrowAfter[A](
      ft: FT,
  )(implicit f: Classifier[T, A], ftoks: FormatTokens): FT = {
    val maybeArrow = ftoks.next(ft)
    if (f(maybeArrow.left)) maybeArrow else ftoks.nextAfterNonComment(maybeArrow)
  }

  def isBlockWithoutBraces(
      t: Term.Block,
  )(implicit ftoks: FormatTokens): Boolean = t.tokens.head match {
    case lb: T.LeftBrace => lb ne ftoks.before(lb).left
    case _ => true
  }

  def existsBlockIfWithoutElse(t: Term.If)(implicit
      ftoks: FormatTokens,
  ): Boolean = existsBlockIfWithoutElse(t.thenp, other = false) ||
    existsBlockIfWithoutElse(t.elsep, ifWithoutElse(t))

  def existsBlockIfWithoutElse(t: Tree, other: => Boolean)(implicit
      ftoks: FormatTokens,
  ): Boolean = t match {
    case x: Term.If => existsBlockIfWithoutElse(x)
    case b @ Term.Block((x: Term.If) :: Nil) => isBlockWithoutBraces(b) &&
      existsBlockIfWithoutElse(x)
    case _ => other
  }

  /** js.native is very special in Scala.js.
    *
    * Context: https://github.com/scalameta/scalafmt/issues/108
    */
  def isJsNative(body: Tree): Boolean = body match {
    case Term.Select(Term.Name("js"), Term.Name("native")) => true
    case _ => false
  }

  def addDialectFeatures(runner: RunnerSettings, tree: Tree): RunnerSettings = {
    val res = mutable
      .Set[RunnerSettings.DialectFeature](runner.dialectFeatures: _*)
    val cnt = res.size

    def isSelectLanguageImport(t: Term.Select): Boolean = t.name.value ==
      "language" &&
      (t.qual match {
        case q: Name => q.value == "scala"
        case q: Term.Select => q.name.value == "scala" &&
          (q.qual match {
            case qq: Name => qq.value == "_root_"
            case _ => false
          })
        case _ => true
      })
    def isLanguageImport(term: Term): Boolean = term match {
      case t: Term.Select => isSelectLanguageImport(t)
      case _ => false
    }
    def findFeature(name: Name): Unit = {}
    def findExperimentalFeature(name: Name): Unit = name.value match {
      case "relaxedLambdaSyntax" => res +=
          RunnerSettings.DialectFeature.relaxedLambdaSyntax
      case _ =>
    }
    @tailrec
    def iter(stats: List[Tree], other: List[List[Tree]]): Unit = stats match {
      case stat :: rest =>
        var newother = other
        stat match {
          case stat: ImportExportStat => stat.importers.foreach { importer =>
              importer.ref match {
                case ref: Term.Select =>
                  if (isSelectLanguageImport(ref)) importer.importees.foreach {
                    case x: Importee.Name => findFeature(x.name)
                    case x: Importee.Rename => findFeature(x.name)
                    case _ =>
                  }
                  else if (
                    ref.name.value == "experimental" &&
                    isLanguageImport(ref.qual)
                  ) importer.importees.foreach {
                    case x: Importee.Name => findExperimentalFeature(x.name)
                    case x: Importee.Rename => findExperimentalFeature(x.name)
                    case _ =>
                  }
                case _ =>
              }
            }
          case stat: Pkg => newother = stat.body.stats :: newother
          case _ =>
        }
        iter(rest, newother)
      case _ => other match {
          case head :: rest => iter(head, rest)
          case _ =>
        }
    }

    tree match {
      case x: Tree.Block => iter(x.stats, Nil)
      case x: Tree.WithStats => iter(x.stats, Nil)
      case _ =>
    }

    if (res.size == cnt) runner else runner.copy(dialectFeatures = res.toSeq)
  }

}
