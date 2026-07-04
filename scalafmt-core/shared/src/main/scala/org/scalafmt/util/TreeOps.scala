package org.scalafmt
package util

import org.scalafmt.Error
import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.LoggerOps._

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.{Token => T}
import scala.meta.trees.Origin

import scala.annotation.tailrec
import scala.collection.mutable

/** Stateless helper functions on `scala.meta.Tree`.
  */
object TreeOps {
  import TokenOps._

  // Shared (allocated once) stable comparator for treeAt's children array,
  // ordering by the child's first-token start offset without boxing.
  private val byChildBegStart: java.util.Comparator[(Tree, T, T)] =
    (a, b) => Integer.compare(a._2.start, b._2.start)

  // A parsed tree's `origin` indexes into the shared full-input token array, so
  // its head/last token (and searches over its token range, or `Wide` searches
  // into the surrounding input) reduce to array reads with no `Tokens` slice.
  // null = not a parsed tree (rewrite-synthesized); callers fall back to a slice.
  def parsedOrigin(tree: Tree): Origin.ParsedPartial = tree.origin match {
    case o: Origin.ParsedPartial => o
    case _ => null
  }

  // Head/last token of a tree WITHOUT allocating a `Tokens` slice. null = none.
  def headTokenOrNull(tree: Tree): T = {
    val o = parsedOrigin(tree)
    if (o eq null) tree.tokens.headOrNull
    else if (o.begTokenIdx < o.endTokenIdx) o.allInputTokens()(o.begTokenIdx)
    else null
  }

  def lastTokenOrNull(tree: Tree): T = {
    val o = parsedOrigin(tree)
    if (o eq null) tree.tokens.lastOrNull
    else if (o.begTokenIdx < o.endTokenIdx) o.allInputTokens()(o.endTokenIdx - 1)
    else null
  }

  // First token of a tree matching `p`, without a slice. null = none.
  def findTokenOrNull(tree: Tree)(p: T => Boolean): T = {
    val o = parsedOrigin(tree)
    if (o eq null) tree.tokens.find(p).orNull
    else {
      val all = o.allInputTokens()
      val end = o.endTokenIdx
      var i = o.begTokenIdx
      while (i < end && !p(all(i))) i += 1
      if (i < end) all(i) else null
    }
  }

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
    )(implicit ftoks: FormatTokens): Option[(Term, (FT, FT))] = tree match {
      case t: Term.ArgClause => unapply(t)
      case _ => None
    }
    def unapply(tree: Term.ArgClause)(implicit
        ftoks: FormatTokens,
    ): Option[(Term, (FT, FT))] = Option(getBraces(tree))

    @inline
    private def getBraces(tree: Term.ArgClause)(implicit
        ftoks: FormatTokens,
    ): (Term, (FT, FT)) = getBraces(tree, tree.values)

    @inline
    private def getBraces[A <: Tree](tree: Tree, values: List[A])(implicit
        ftoks: FormatTokens,
    ): (A, (FT, FT)) = values match {
      case arg :: Nil => getBracesNested(tree, values).nnMap((arg, _))
      case _ => null
    }

    @inline
    private def getBracesNested(tree: Tree, values: List[Tree])(implicit
        ftoks: FormatTokens,
    ): (FT, FT) = values match {
      case _ :: Nil => ftoks.getBracesIfEnclosed(tree) ??
          (tree.parent match {
            case Some(p: Term.ArgClause) => getBracesNested(p, p.values)
            case Some(p: Term.Block) => getBracesNested(p, p.stats)
            case _ => null
          })
      case _ => null
    }

    def orBlock(tree: Tree)(implicit ftoks: FormatTokens): (Stat, (FT, FT)) =
      tree match {
        case t: Term.ArgClause => getBraces(t)
        case t: Term.Block => getBraces(t, t.stats)
        case _ => null
      }

    object OrBlock {
      def unapply(tree: Tree)(implicit
          ftoks: FormatTokens,
      ): Option[(Stat, (FT, FT))] = Option(orBlock(tree))
    }
  }

  @tailrec
  def isBlockFunction(fun: Tree)(implicit ftoks: FormatTokens): Boolean =
    fun.parent match {
      case Some(p: Term.FunctionLike) => isBlockFunction(p)
      case Some(p @ Term.Block(`fun` :: Nil)) => ftoks.getHead(p).left
          .is[T.LeftBrace] || isBlockFunction(p)
      case Some(SingleArgInBraces(`fun`, _)) => true
      case _ => false
    }

  def isFunctionWithBraces(fun: Member.Function)(implicit
      ftoks: FormatTokens,
  ): Boolean = fun.parent.exists(isExprWithParentInBraces(fun))

  def isExprWithParentInBraces(expr: Tree)(parent: Tree)(implicit
      ftoks: FormatTokens,
  ): Boolean = SingleArgInBraces.orBlock(parent).nnHas(_._1 eq expr)

  /** Finds matching delimiters [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  // java.util.HashMap so consumers can `get` a value-or-null without a `Some`
  def getMatchingDelims[K, V](
      coll: IndexedSeq[V],
  )(key: V => K)(f: V => T): java.util.HashMap[K, V] = {
    def show(tok: T): String = s"[${tok.end}]$tok"
    val ret = new java.util.HashMap[K, V]
    var stack = List.empty[(T, V)]
    coll.foreach { elem =>
      f(elem) match {
        case lt @ (_: T.OpenDelim | _: T.Xml.Start | _: T.Xml.SpliceStart |
            _: T.Interpolation.Start | _: T.Interpolation.SpliceStart) =>
          stack = (lt, elem) :: stack
        case rt @ (_: T.CloseDelim | _: T.Xml.End | _: T.Xml.SpliceEnd |
            _: T.Interpolation.End | _: T.Interpolation.SpliceEnd) =>
          require(stack.nonEmpty, s"Orphan closing delim ${show(rt)}")
          val (lt, ltElem) = stack.head
          require(
            checkValidDelims(lt, rt),
            s"Mismatched delims (${show(lt)}, ${show(rt)})",
          )
          ret.put(key(ltElem), elem)
          ret.put(key(elem), ltElem)
          stack = stack.tail
        case _ =>
      }
    }
    if (stack.nonEmpty) throw new IllegalArgumentException(
      stack.map { case (x, _) => show(x) }.mkString("Orphan delims: ", ", ", ""),
    )
    ret
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
  def findTreeOrParent(tree: Tree)(predMaybe: Tree => MaybeBool): Tree =
    findTreeEx(tree)(t =>
      predMaybe(t) match {
        case MaybeBool.Maybe => t.parentOrNoTree
        case MaybeBool.True => null
        case MaybeBool.False => Tree.NoTree
      },
    )

  /** Returns first tree which matches the given predicate. The predicate
    * returns None to indicate failure; or the tree to recurse to; if the tree
    * is null (or the same as current tree), the current tree is returned.
    */
  @tailrec
  def findTreeEx(tree: Tree)(pred: Tree => Tree): Tree = pred(tree) match {
    case Tree.NoTree => null
    case null | `tree` => tree
    case r => findTreeEx(r)(pred)
  }

  def findTreeOrParentSimple(tree: Tree, flag: Boolean = true)(
      pred: Tree => Boolean,
  ): Tree = findTreeOrParent(tree)(x =>
    if (pred(x) == flag) MaybeBool.True else MaybeBool.Maybe,
  )

  /** Returns first ancestor whose parent matches the given predicate. The
    * predicate returns None to continue with the parent, or the boolean match
    * flag, which terminates the search.
    */
  /** Returns (or `null`) the first ancestor whose parent matches the predicate
    * (`None` = continue to parent, `Some(true)` = match-stop, `Some(false)` =
    * fail). Walks the parent chain directly and returns `Tree`/`null` rather
    * than going through `findTreeWithParentEx` with an `Option[Tree]` mapping
    * lambda, which allocated a `Some` on every upward step (hot path).
    */
  def findTreeWithParent(tree: Tree)(pred: Tree => MaybeBool): Tree = {
    var child = tree
    var result: Tree = null
    var searching = true
    while (searching) child.parent match {
      case None => searching = false
      case Some(p) => pred(p) match {
          case MaybeBool.Maybe => child = p
          case MaybeBool.True => result = child; searching = false
          case MaybeBool.False => searching = false
        }
    }
    result
  }

  /** Returns first ancestor whose parent matches the given predicate. The
    * predicate returns None to indicate failure; or the tree to recurse to; if
    * the recurse-to tree is null, the current tree is returned.
    */
  @tailrec
  def findTreeWithParentEx(tree: Tree)(pred: Tree => Tree): Tree =
    tree.parent match {
      case None => null
      case Some(p) => pred(p) match {
          case Tree.NoTree => null
          case null => tree
          case r => findTreeWithParentEx(r)(pred)
        }
    }

  def findTreeWithParentSimple(tree: Tree, flag: Boolean = true)(
      pred: Tree => Boolean,
  ): Tree = findTreeWithParent(tree)(x =>
    if (pred(x) == flag) MaybeBool.True else MaybeBool.Maybe,
  )

  /** Returns first ancestor with a parent of a given type (or `null`).
    */
  def findTreeWithParentOfType[A <: Tree](tree: Tree)(implicit
      classifier: Classifier[Tree, A],
  ): Tree = findTreeWithParentSimple(tree)(classifier.apply)

  /** Returns true if a matching ancestor of a given type exists.
    */
  @inline
  def existsParentOfType[A <: Tree](tree: Tree)(implicit
      classifier: Classifier[Tree, A],
  ): Boolean = findTreeWithParentOfType[A](tree) ne null

  @tailrec
  def defDefBody(tree: Tree): Tree = tree match {
    case d: Defn with Tree.WithBody => d.body
    case d: Defn with Stat.WithTemplate => d.templ.body
    case t: Ctor.Block => t
    case t: Ctor.Secondary => t.body
    case _: Ctor.Primary | _: Pat.Var | _: Term.Name => tree.parent match {
        case Some(p) => defDefBody(p)
        case _ => null
      }
    case _ => null
  }
  @tailrec
  def defDefBodyParent(tree: Tree): Tree = tree.parent match {
    case Some(p: Member.ParamClauseGroup) => defDefBodyParent(p)
    case Some(p) => defDefBody(p)
    case None => null
  }

  // invoked ONLY for colon
  def colonDeclType(tree: Tree): Type = tree match {
    case d: Defn.Given =>
      val trees = d.templ.inits
      if (trees.isEmpty) null else trees.head.tpe
    case d: Tree.WithDeclTpe => d.decltpe // all are Decl or Defn
    case d: Tree.WithDeclTpeOpt with Defn => d.decltpe.orNull
    case _ => null
  }

  object ColonDeclType {
    def unapply(tree: Tree): Option[Type] = Option(colonDeclType(tree))
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
  def isTokenHeadOrBefore(token: T, owner: Tree): Boolean = owner.begOffset >=
    token.start

  @inline
  def isTokenLastOrAfter(token: T, owner: Tree): Boolean = owner.endOffset <=
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

  def getSingleArgOnLeftBraceOnLeft[A >: Null](
      f: (Term.ArgClause, Stat) => A,
  )(ft: FT)(implicit ftoks: FormatTokens): A = ft.leftOwner match {
    case ac: Term.ArgClause =>
      val stat = ac.values match {
        case (t: Term.Block) :: Nil if ftoks.getHead(t) eq ft =>
          getBlockSingleStat(t)
        case t :: Nil => t
        case _ => null
      }
      if (stat eq null) null else f(ac, stat)
    case t: Term => t.parent match {
        case Some(ac: Term.ArgClause) if ac.values.lengthCompare(1) == 0 =>
          val stat = t match {
            case t: Term.Block if ftoks.getHead(ac) eq ft =>
              getBlockSingleStat(t)
            case _ => t
          }
          if (stat eq null) null else f(ac, stat)
        case _ => null
      }
    case _ => null
  }

  def getSingleArgLambdaPenalties(ac: Term.ArgClause, arg: Stat): (Int, Int) =
    if (arg.is[Term.FunctionLike]) (nestedApplies(ac), 2)
    else ac.parent match {
      case Some(p: Term.Apply) => (nestedApplies(p), treeDepth(p.fun))
      case _ => null
    }

  def getLambdaPenaltiesOnLeftBraceOnLeft(ft: FormatToken)(implicit
      ftoks: FormatTokens,
  ): (Int, Int) = getSingleArgOnLeftBraceOnLeft(getSingleArgLambdaPenalties)(ft)

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
      res: Member.Function = null,
  )(implicit ftoks: FormatTokens, style: ScalafmtConfig): Member.Function = {
    val nextres = if (canBreakAfterFuncArrow(first)) first else res
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
  final def asInfixApp(tree: Tree): Member.Infix = InfixApp.get(tree)

  @inline
  final def isInfixApp(tree: Tree): Boolean = tree.is[Member.Infix]

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
  ): Member.Infix = tree.parent match {
    case Some(t: Member.ArgClause) => findNextInfixInParent(t, scope)
    case Some(t: Term.Block) if !ftoks.isEnclosedInBraces(t) =>
      findNextInfixInParent(t, scope)
    case Some(t: Member.Infix) if tree ne scope =>
      if (t.lhs eq tree) t else findNextInfixInParent(t, scope)
    case _ => null
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
  def isProcedureSyntaxDeclTpe(tpe: Type): Boolean = headTokenOrNull(tpe) eq
    null

  def isProcedureSyntax(defn: Defn.Def): Boolean = defn.decltpe
    .exists(isProcedureSyntaxDeclTpe)

  def isXmlBrace(owner: Tree): Boolean = owner match {
    case _: Term.Xml | _: Pat.Xml => true
    case b: Term.Block => b.parent.is[Term.Xml]
    case _ => false
  }

  def getAssignAtSingleArgCallSite(args: Seq[Tree]): Term.Assign = args match {
    case Seq(fun: Term.Assign) => fun
    case _ => null
  }

  @inline
  def isSeqSingle(seq: Seq[_]): Boolean = seq.lengthCompare(1) == 0

  @inline
  def isSeqMulti(seq: Seq[_]): Boolean = seq.lengthCompare(1) > 0

  @inline
  def isSingleStatBlock(tree: Term.Block): Boolean = isSeqSingle(tree.stats)

  @inline
  def isMultiStatBlock(tree: Term.Block): Boolean = isSeqMulti(tree.stats)

  def getSingleElement[A >: Null](elements: List[A]): A = elements match {
    case elem :: Nil => elem
    case _ => null
  }

  @inline
  def getSingleElement(tree: Tree.Block): Tree = getSingleElement(tree.stats)

  @inline
  def hasSingleElement(tree: Tree.Block, value: Tree): Boolean =
    getSingleElement(tree) eq value

  @inline
  def hasSingleElement(tree: Member.SyntaxValuesClause, value: Tree): Boolean =
    getSingleElement(tree.values) eq value

  def getBlockSingleStat(b: Term.Block): Stat = b.stats match {
    case stat :: Nil => stat
    case _ => null
  }

  def isTreeMultiStatBlock(tree: Tree): Boolean = tree match {
    case t: Term.Block => isMultiStatBlock(t)
    case _ => false
  }

  @tailrec
  def getTreeSingleExpr(tree: Tree): Term = tree match {
    case t: Term.Block => t.stats match {
        case stat :: Nil => getTreeSingleExpr(stat)
        case _ => null
      }
    case t: Term => t
    case _ => null
  }

  def isTreeSingleExpr(tree: Tree): Boolean = getTreeSingleExpr(tree) ne null

  /* An end marker is really more like a closing brace for formatting purposes
   * (but not when rewriting) so we should ignore it when considering whether a
   * block contains only a single statement. NB: in FormatWriter, when choosing
   * to insert or remove end markers, we avoid such borderline cases.
   */
  def getSingleStatExceptEndMarker[A >: Null <: Tree](ss: List[A]): A = ss match {
    case s :: rs if (rs match {
          case Nil | (_: Term.EndMarker) :: Nil => true
          case _ => false
        }) => s
    case _ => null
  }

  def getSingleStatExceptEndMarker(t: Tree): Tree = t match {
    case Term.Block(s) => getSingleStatExceptEndMarker(s)
    case _ => t
  }

  def getTreeSingleStat(t: Tree): Tree = t match {
    case b: Term.Block => getBlockSingleStat(b)
    case _ => t
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
    case b: Term.Block =>
      val s = getSingleStatExceptEndMarker(b.stats)
      if ((s eq null) || ftoks.isEnclosedInBraces(b)) t else getBlockStat(s)
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
    val beg = m.begOffset
    beg < ownerStart || beg == m.endOffset
  }

  def noExplicitImplicit(ownerStart: Int, orElse: Boolean)(m: Mod): Boolean =
    m match {
      case m: Mod.Implicit => noExplicitImplicit(m, ownerStart)
      case _ => orElse
    }

  def noExplicitImplicit(param: Term.Param): Boolean = {
    val pStart = param.begOffset
    param.mods.forall(noExplicitImplicit(pStart, true))
  }

  def getImplicitParamList(kwOwner: Tree): Member.SyntaxValuesClause =
    kwOwner.parent match {
      case Some(v @ Term.ArgClause(_, Some(`kwOwner`))) => v
      case Some(v @ Term.ParamClause(_ :: rest, Some(`kwOwner`)))
          if !kwOwner.is[Mod.Implicit] || rest.isEmpty ||
            rest.exists(noExplicitImplicit) => v
      case _ => null
    }

  def hasImplicitParamList(kwOwner: Tree): Boolean =
    getImplicitParamList(kwOwner) ne null

  def getEndOfFirstCall(tree: Tree)(implicit ftoks: FormatTokens): FT = {
    @tailrec
    def traverse(tree: Tree, res: Tree): Tree = tree match {
      case t: Term.SelectLike if res ne null => traverse(t.qual, t.qual)
      case t: Term.ApplyType => traverse(t.fun, t)
      case t: Member.Apply => traverse(t.fun, t.fun)
      case t: Init => traverse(t.tpe, t.tpe)
      case Term.Block(arg :: Nil) if !ftoks.isEnclosedInBraces(tree) =>
        traverse(arg, res)
      case _ => res
    }
    traverse(tree, null).nnMap(ftoks.getLast)
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
  def findInterpolate(tree: Tree): Term.Interpolate = tree match {
    case ti: Term.Interpolate => ti
    case _ => tree.parent match {
        case Some(p) => findInterpolate(p)
        case _ => null
      }
  }

  def findArgAfter(end: Int, trees: Seq[Tree]): Tree = {
    val iter = trees.iterator
    while (iter.hasNext) {
      val t = iter.next()
      if (t.begOffset >= end) return t
    }
    null
  }

  def getStripMarginCharForInterpolate(tree: Tree): Option[Char] = {
    val interp = findInterpolate(tree)
    if (interp eq null) None else getStripMarginChar(interp)
  }

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
  def findFirstTreeBetween(tree: Tree, beg: T, end: T): Tree = {
    def isWithinRange(t: Tree): Boolean = {
      val h = headTokenOrNull(t)
      (h ne null) && h.start >= beg.start && lastTokenOrNull(t).end <= end.end
    }
    def matches(t: Tree): Boolean = {
      val h = headTokenOrNull(t)
      (h ne null) && {
        val le = lastTokenOrNull(t).end
        h.start >= beg.start && le <= end.end ||
        h.start <= beg.start && le >= end.end
      }
    }
    if (isWithinRange(tree)) tree
    else tree.children.find(matches) match {
      case Some(c) => findFirstTreeBetween(c, beg, end)
      case _ => null
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

  def getTemplateGroups(template: Template): Seq[List[Tree]] = {
    val groups = Seq(template.inits, template.derives).filter(_.nonEmpty)
    if (groups.isEmpty) null else groups
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

  def findEnclosedBetweenParens(lt: T, rt: T, tree: Tree): Tree = {
    val beforeParens = lt.start
    val afterParens = rt.end
    val found = beforeParens <= tree.begOffset && tree.endOffset <= afterParens
    if (found) tree
    else {
      // `result` (null until decided) + a decided-flag avoids a non-local return
      // out of the `foreachChild` closure (which would allocate/throw a
      // NonLocalReturnControl); we just skip work once decided.
      var candidate: Tree = null
      var result: Tree = null
      tree.foreachChild { child =>
        if (result eq null)
          if (candidate ne null) result =
            if (child.begOffset >= afterParens) candidate else Tree.NoTree
          else {
            val headEnd = child.endOffset
            if (headEnd > beforeParens)
              if (headEnd > afterParens || child.begOffset >= headEnd)
                result = Tree.NoTree
              else candidate = child
          }
      }
      if (result eq Tree.NoTree) null
      else if (result ne null) result
      else candidate
    }
  }

  def getStyleAndOwners(
      topSourceTree: Tree,
      baseStyle: ScalafmtConfig,
  ): (ScalafmtConfig, Array[Tree]) = {
    var termInfixCount = 0
    var typeInfixCount = 0
    var patInfixCount = 0
    val allTokens = topSourceTree.tokens
    // Lookup table from token index to its closest scala.meta tree. Token
    // indices are dense (== position in `allTokens`), so an array beats a
    // boxed-Long-keyed map and skips per-token hashing; every token gets an
    // owner, so the array is no sparser than the map was.
    val ownersArr = new Array[Tree](allTokens.length)
    @inline
    def setOwner(idx: Int, tree: Tree): Unit = ownersArr(idx) = tree

    var prevParens: List[Int] = Nil

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
      // Collect qualifying children into a pre-sized array (childrenCount is an
      // upper bound; some are filtered out), then sort the used prefix in place.
      // Uses foreachChild so scalameta never materializes its `children` List,
      // and java.util.Arrays.sort (stable) instead of List.sortWith (which
      // rebuilds a List). Leaf nodes (childrenCount == 0) skip all of it.
      val childCap = elem.childrenCount
      val allChildren =
        if (childCap == 0) null else new Array[(Tree, T, T)](childCap)
      var childCount = 0
      if (allChildren ne null) elem.foreachChild { x =>
        val beg = headTokenOrNull(x)
        if ((beg ne null) && beg.start >= treeBeg) { // sometimes with implicit
          val end = lastTokenOrNull(x)
          if (end.end <= treeEnd) {
            allChildren(childCount) = (x, beg, end)
            childCount += 1
          }
        }
      }
      if (childCount > 1) java.util.Arrays
        .sort(allChildren, 0, childCount, byChildBegStart)

      if (childCount == 0) {
        @tailrec
        def tokenAt(idx: Int): Int = {
          val tok = allTokens(idx)
          setOwner(idx, elem)
          val nextIdx = idx + 1
          if (tok eq elemEnd) nextIdx else tokenAt(nextIdx)
        }
        tokenAt(elemIdx)
      } else {
        val firstEntry = allChildren(0)
        val firstChild = firstEntry._1
        var nextChild = firstChild
        var nextChildBeg = firstEntry._2
        var nextChildEnd = firstEntry._3
        var childIdx = 1
        var prevChild: Tree = null
        var prevLPs = outerPrevLPs
        var prevComma: Int = -1

        // `excludeRightParen` takes prevLPs/prevChild as params (rather than
        // capturing the loop vars) so the loop's mutable state is never closed
        // over -- otherwise each captured-and-mutated var would be lifted to a
        // heap Ref cell per node.
        def excludeRightParen(tok: T, prevLPs: Int, prevChild: Tree): Boolean =
          elem match {
            case t: Term.If => prevLPs == 1 && prevChild == t.cond // `expr` after `mods`
            case _: Term.While | _: Term.ForClause => prevLPs == 1 &&
              prevChild == firstChild // `expr` is first
            case _: Member.SyntaxValuesClause | _: Member.Tuple | _: Term.Do |
                _: Term.AnonymousFunction => elemEnd eq tok
            case t: Init => prevChild ne t.tpe // include tpe
            case _: Ctor.Primary | _: Term.EnumeratorsBlock => true
            case _ => false
          }

        // Explicit while-loop (not a nested @tailrec def) so idx/nextChild/...
        // stay stack-local; a nested def would capture the mutated vars and
        // lift each to a heap Ref cell on every treeAt call (per node).
        var idx = elemIdx
        var result = -1
        while (result < 0)
          if (idx > 0 && (elemEnd eq allTokens(idx - 1))) result = idx
          else {
            val tok = allTokens(idx)
            if (tok eq nextChildBeg) {
              if (prevChild != null) prevLPs = 0
              prevChild = nextChild
              val nextIdx = treeAt(idx, nextChild, tok, nextChildEnd, prevLPs)
              if (childIdx < childCount) {
                val c = allChildren(childIdx)
                childIdx += 1
                nextChild = c._1
                nextChildBeg = c._2
                nextChildEnd = c._3
              } else nextChildBeg = null
              prevComma = -1
              idx = nextIdx
            } else {
              if (prevParens.nonEmpty && tok.is[T.RightParen]) {
                if (
                  prevChild == null || prevLPs <= 0 ||
                  excludeRightParen(tok, prevLPs, prevChild)
                ) setOwner(idx, elem)
                else {
                  setOwner(idx, prevChild)
                  setOwner(prevParens.head, prevChild)
                  if (prevComma >= 0) setOwner(prevComma, prevChild)
                }
                prevLPs -= 1
                prevParens = prevParens.tail
                prevComma = -1
              } else if (tok.is[T.Comma]) {
                prevComma = idx
                setOwner(idx, elem)
              } else {
                setOwner(idx, elem)
                if (!tok.is[T.Trivia] && !tok.isEmpty) {
                  prevComma = -1
                  prevChild = null
                  if (tok.is[T.LeftParen]) {
                    prevLPs += 1
                    prevParens = idx :: prevParens
                  } else prevLPs = 0
                }
              }
              idx = idx + 1
            }
          }
        result
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
    (initStyle, ownersArr)
  }

  def isFewerBraces(
      tree: Term.Apply,
  )(implicit dialect: Dialect, ftoks: FormatTokens): Boolean =
    dialect.allowFewerBraces && ftoks.getHead(tree.argClause).left.is[T.Colon]

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
    case t => headTokenOrNull(t) eq null
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
  def getBlockWithNonSingleTermStat(t: Term.Block): Term.Block = t.stats match {
    case (x: Term.Block) :: Nil => getBlockWithNonSingleTermStat(x)
    case (_: Term) :: Nil => null
    case _ :: _ => t
    case _ => null
  }

  // look for arrow before body, if any, else after params
  def getFuncArrow(term: Member.Function)(implicit ftoks: FormatTokens): FT =
    ftoks.tokenBefore(term.body) ?? ftoks.tokenAfter(term.paramClause)
      .nnMap(getArrowAfter[T.FunctionArrow])

  // look for arrow before body, if any, else after cond/pat
  def getCaseArrow(term: Case)(implicit ftoks: FormatTokens): FT = ftoks
    .tokenBefore(term.body) ??
    getArrowAfter[T.RightArrow](ftoks.tokenAfter(term.cond.getOrElse(term.pat)))

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
  )(implicit ftoks: FormatTokens): Boolean = headTokenOrNull(t) match {
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

  def isCurlyWithNextKeyword(rbOwner: Tree, rft: FT): Boolean = rbOwner.parent
    .contains(rft.rightOwner)

  def isCurlyWithNextKeyword(lft: FT, rft: FT): Boolean =
    isCurlyWithNextKeyword(lft.leftOwner, rft)

}
