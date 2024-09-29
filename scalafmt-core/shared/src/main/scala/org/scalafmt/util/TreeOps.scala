package org.scalafmt.util

import org.scalafmt.Error
import org.scalafmt.config._
import org.scalafmt.internal._
import org.scalafmt.util.InfixApp._
import org.scalafmt.util.LoggerOps._

import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token.{Space => _, _}
import scala.meta.tokens.Tokens

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
    def unapply(tree: Tree)(implicit
        ftoks: FormatTokens,
    ): Option[(FormatToken, Term, FormatToken)] = tree match {
      case t: Term.ArgClause => unapply(t)
      case _ => None
    }
    def unapply(tree: Term.ArgClause)(implicit
        ftoks: FormatTokens,
    ): Option[(FormatToken, Term, FormatToken)] = getBraces(tree, tree.values)

    @inline
    private def getBraces[A](tree: Tree, values: List[A])(implicit
        ftoks: FormatTokens,
    ): Option[(FormatToken, A, FormatToken)] = values match {
      case arg :: Nil => ftoks.getBracesIfEnclosed(tree).map { case (b, e) =>
          (b, arg, e)
        }
      case _ => None
    }

    def orBlock(tree: Tree)(implicit
        ftoks: FormatTokens,
    ): Option[(FormatToken, Stat, FormatToken)] = tree match {
      case t: Term.ArgClause => unapply(t)
      case t: Term.Block => getBraces(t, t.stats)
      case _ => None
    }

    object OrBlock {
      def unapply(tree: Tree)(implicit
          ftoks: FormatTokens,
      ): Option[(FormatToken, Stat, FormatToken)] = orBlock(tree)
    }
  }

  @tailrec
  def isBlockFunction(fun: Term)(implicit ftoks: FormatTokens): Boolean =
    fun.parent match {
      case Some(p: Term.FunctionTerm) => isBlockFunction(p)
      case Some(p @ Term.Block(`fun` :: Nil)) => ftoks.getHead(p).left
          .is[Token.LeftBrace] || isBlockFunction(p)
      case Some(SingleArgInBraces(_, `fun`, _)) => true
      case _ => false
    }

  def isFunctionWithBraces(fun: Term.FunctionTerm)(implicit
      ftoks: FormatTokens,
  ): Boolean = fun.parent.exists(isExprWithParentInBraces(fun))

  def isExprWithParentInBraces(expr: Tree)(parent: Tree)(implicit
      ftoks: FormatTokens,
  ): Boolean = SingleArgInBraces.orBlock(parent).exists(_._2 eq expr)

  /** Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses[A](
      coll: Iterable[A],
  )(f: A => Token): Map[TokenHash, A] = {
    val ret = Map.newBuilder[TokenHash, A]
    var stack = List.empty[(Token, A)]
    coll.foreach { elem =>
      f(elem) match {
        case open @ (_: Token.OpenDelim | _: Interpolation.Start |
            _: Xml.Start | _: Xml.SpliceStart) => stack = (open, elem) :: stack
        case close @ (_: Token.CloseDelim | _: Interpolation.End | _: Xml.End |
            _: Xml.SpliceEnd) =>
          val (open, openElem) = stack.head
          assertValidParens(open, close)
          ret += hash(open) -> elem
          ret += hash(close) -> openElem
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

  def assertValidParens(open: Token, close: Token): Unit = (open, close) match {
    case (Interpolation.Start(), Interpolation.End()) =>
    case (Xml.Start(), Xml.End()) =>
    case (Xml.SpliceStart(), Xml.SpliceEnd()) =>
    case (LeftBrace(), RightBrace()) =>
    case (LeftBracket(), RightBracket()) =>
    case (LeftParen(), RightParen()) =>
    case (o, c) =>
      throw new IllegalArgumentException(s"Mismatching parens ($o, $c)")
  }

  final def childOf(child: Tree, tree: Tree): Boolean =
    findTreeOrParentSimple(child)(_ eq tree).isDefined

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
  )(pred: Tree => Option[Boolean]): Option[Tree] = findTreeEx(tree) { t =>
    pred(t) match {
      case None => t.parent
      case Some(true) => Some(null)
      case Some(false) => None
    }
  }

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
  ): Option[Tree] = findTreeWithParentEx(tree) { t =>
    pred(t) match {
      case None => Some(t)
      case Some(true) => Some(null)
      case Some(false) => None
    }
  }

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

  val ColonDeclTpeLeft =
    new FormatToken.ExtractFromMeta(x => colonDeclType(x.leftOwner))
  val ColonDeclTpeRight =
    new FormatToken.ExtractFromMeta(x => colonDeclType(x.rightOwner))

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
  def isTokenHeadOrBefore(token: Token, owner: Tree): Boolean =
    isTokenHeadOrBefore(token, owner.pos)

  @inline
  def isTokenHeadOrBefore(token: Token, pos: Position): Boolean = pos.start >=
    token.start

  @inline
  def isTokenLastOrAfter(token: Token, owner: Tree): Boolean =
    isTokenLastOrAfter(token, owner.pos)

  @inline
  def isTokenLastOrAfter(token: Token, pos: Position): Boolean = pos.end <=
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

  def isTuple(tree: Tree): Boolean = tree.is[Member.Tuple]

  def noSpaceBeforeOpeningParen(tree: Tree): Boolean = tree match {
    case _: Term.Super => true
    case t: Member.ArgClause => !t.parent.is[Member.Infix]
    case _: Member.ParamClause => tree.parent.exists {
        case _: Term.FunctionTerm => false
        case t: Ctor.Primary => t.mods.isEmpty ||
          !t.paramClauses.headOption.contains(tree)
        case _ => true
      }
    case _ => false
  }

  def isModPrivateProtected(tree: Tree): Boolean = tree match {
    case _: Mod.Private | _: Mod.Protected => true
    case _ => false
  }

  val DefValAssignLeft = new FormatToken.ExtractFromMeta(_.leftOwner match {
    case _: Enumerator => None // it's WithBody
    case t: Ctor.Secondary => Some(t.body.init)
    case t: Tree.WithBody => Some(t.body)
    case t: Term.Param => t.default
    case _ => None
  })

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
    case Member.SyntaxValuesClause(v) => maxTreeDepth(v)
    case _ =>
      val children = tree.children
      if (children.isEmpty) 0 else 1 + maxTreeDepth(children)
  }

  def maxTreeDepth(trees: Seq[Tree]): Int = trees.foldLeft(0) { case (res, t) =>
    math.max(res, treeDepth(t))
  }

  @tailrec
  final def lastLambda(
      first: Term.FunctionTerm,
  )(implicit ftoks: FormatTokens): Term.FunctionTerm = first.body match {
    case child: Term.FunctionTerm => lastLambda(child)
    case b @ Term.Block((child: Term.FunctionTerm) :: Nil)
        if !ftoks.getHead(b).left.is[Token.LeftBrace] => lastLambda(child)
    case _ => first
  }

  @inline
  final def isInfixOp(tree: Tree): Boolean = AsInfixOp.unapply(tree).isDefined

  object AsInfixOp {
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
  def findNextInfixInParent(tree: Tree, scope: Tree): Option[Name] =
    tree.parent match {
      case Some(t: Member.ArgClause) => findNextInfixInParent(t, scope)
      case Some(t: Member.Infix) if tree ne scope =>
        if (t.lhs eq tree) Some(t.op) else findNextInfixInParent(t, scope)
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
  def isProcedureSyntax(defn: Defn.Def): Boolean = defn.decltpe
    .exists(_.tokens.isEmpty)

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
  def getTreeSingleExpr(tree: Tree): Option[Tree] = tree match {
    case t: Term.Block => t.stats match {
        case stat :: Nil => getTreeSingleExpr(stat)
        case _ => None
      }
    case _: Defn => None
    case t => Some(t)
  }

  def isTreeSingleExpr(tree: Tree): Boolean = getTreeSingleExpr(tree).isDefined

  /* An end marker is really more like a closing brace for formatting purposes
   * (but not when rewriting) so we should ignore it when considering whether a
   * block contains only a single statement. NB: in FormatWriter, when choosing
   * to insert or remove end markers, we avoid such borderline cases.
   */
  def getSingleStatExceptEndMarker[A <: Tree](ss: List[A]): Option[A] =
    ss match {
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

  def isChildOfCaseClause(tree: Tree): Boolean = findTreeWithParent(tree) {
    case t: Case => Some(tree ne t.body)
    case _: Pat | _: Pat.ArgClause => None
    case _ => Some(false)
  }.isDefined

  object EndOfFirstCall {
    def unapply(tree: Tree): Option[Token] = traverse(tree, None)
      .map(_.tokens.last)

    @tailrec
    private def traverse(tree: Tree, res: Option[Tree]): Option[Tree] =
      tree match {
        case t: Term.Select if res.isDefined => traverse(t.qual, Some(t.qual))
        case t: Term.ApplyType => traverse(t.fun, Some(t))
        case t: Member.Apply => traverse(t.fun, Some(t.fun))
        case t: Init => traverse(t.tpe, Some(t.tpe))
        case _ => res
      }
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
    case Some(ts: Term.Select) if ts.name.value == "stripMargin" =>
      ts.parent match {
        case Some(Term.Apply.Initial(_, List(arg: Lit.Char))) => Some(arg.value)
        case _ => Some('|')
      }
    case _ => None
  }

  @inline
  def isTripleQuote(syntax: String): Boolean = syntax.startsWith("\"\"\"")

  @tailrec
  def findFirstTreeBetween(tree: Tree, beg: Token, end: Token): Option[Tree] = {
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
  def isCaseBodyABlock(ft: FormatToken, caseStat: CaseTree): Boolean = ft.right
    .is[Token.LeftBrace] && (caseStat.body eq ft.meta.rightOwner)

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
      left: Token,
      ft: FormatToken,
      whenNL: Boolean = true,
  ): Boolean = {
    def owner = ft.meta.rightOwner
    def isArgOrParamClauseSite(tree: Tree) = !whenNL || isArgClauseSite(tree) ||
      isParamClauseSite(tree)
    // skip empty parens/braces/brackets
    ft.right match {
      case _: Token.RightBrace => !left.is[Token.LeftBrace] && owner.is[Importer]
      case _: Token.RightParen => !left.is[Token.LeftParen] &&
        isArgOrParamClauseSite(owner)
      case _: Token.RightBracket => !left.is[Token.LeftBracket] &&
        isArgOrParamClauseSite(owner)
      case _ => false
    }
  }

  def findEnclosedBetweenParens(
      lt: Token,
      rt: Token,
      tree: Tree,
  ): Option[Tree] = {
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
    var infixCount = 0
    // Creates lookup table from token offset to its closest scala.meta tree
    val ownersMap = HashMap.newBuilder[TokenHash, Tree]
    @inline
    def setOwner(tok: Token, tree: Tree): Unit = ownersMap += hash(tok) -> tree

    val allTokens = topSourceTree.tokens
    var prevParens: List[Token] = Nil

    def treeAt(
        elemIdx: Int,
        elem: Tree,
        elemBeg: Token,
        elemEnd: Token,
        outerPrevLPs: Int,
    ): Int = {
      if (TreeOps.isInfixApp(elem)) infixCount += 1

      val treeBeg = elemBeg.start
      val treeEnd = elemEnd.end
      val allChildren: List[(Tree, Token, Token)] = {
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
          var prevComma: Token = null

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

    val checkedNewlines = baseStyle.newlines.checkInfixConfig(infixCount)
    val initStyle =
      if (checkedNewlines eq baseStyle.newlines) baseStyle
      else baseStyle.copy(newlines = checkedNewlines)
    (initStyle, ownersMap.result())
  }

  val ParamClauseParentLeft =
    new FormatToken.ExtractFromMeta(_.leftOwner match {
      case ParamClauseParent(p) => Some(p)
      case _ => None
    })

  val LambdaAtSingleArgCallSite =
    new FormatToken.ExtractFromMeta(_.leftOwner match {
      case Term.ArgClause((fun: Term.FunctionTerm) :: Nil, _) => Some(fun)
      case _ => None
    })

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

  def isTreeOrBlockParent(owner: Tree)(pred: Tree => Boolean): Boolean =
    if (owner.is[Term.Block]) owner.parent.exists(pred) else pred(owner)

  def xmlSpace(owner: Tree): Modification =
    Space(!isTreeOrBlockParent(owner)(_.isAny[Term.Xml, Pat.Xml]))

  def isEmptyTree(tree: Tree): Boolean = tree match {
    case t: Term.Block => t.stats.isEmpty
    case t => t.tokens.isEmpty
  }

  def isEmptyFunctionBody(tree: Tree): Boolean = tree match {
    case t: Term.FunctionTerm => isEmptyTree(t.body)
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

  def getArgs(owner: Tree): Seq[Tree] = getArgsPartial
    .applyOrElse(owner, throwUnexpectedGetArgs)

}
