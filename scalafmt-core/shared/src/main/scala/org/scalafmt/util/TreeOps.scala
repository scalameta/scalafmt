package org.scalafmt.util

import scala.annotation.tailrec
import scala.collection.immutable.HashMap
import scala.collection.mutable
import scala.meta._
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens
import scala.reflect.ClassTag

import org.scalafmt.Error
import org.scalafmt.config.{DanglingParentheses, ScalafmtConfig}
import org.scalafmt.internal.{FormatToken, FormatTokens}
import org.scalafmt.util.InfixApp._

/** Stateless helper functions on `scala.meta.Tree`.
  */
object TreeOps {
  import TokenOps._

  @tailrec
  def topTypeWith(typeWith: Type.With): Type.With =
    typeWith.parent match {
      case Some(t: Type.With) => topTypeWith(t)
      case _ => typeWith
    }

  @tailrec
  def withChain(top: Tree, res: Seq[Type.With] = Seq.empty): Seq[Type.With] =
    top match {
      case t: Type.With => withChain(t.lhs, t +: res)
      case _ => res
    }

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

  @tailrec
  def isBlockFunction(fun: Term.FunctionTerm): Boolean =
    fun.parent match {
      case Some(Term.Block(`fun` :: Nil)) => true
      case Some(next: Term.FunctionTerm) => isBlockFunction(next)
      case _ => false
    }

  def isFunctionWithBraces(fun: Term.Function): Boolean =
    fun.parent match {
      case Some(Term.Block(`fun` :: Nil)) => true
      case _ => false
    }

  def extractStatementsIfAny(tree: Tree): Seq[Tree] =
    tree match {
      case b: Term.Block => b.stats
      case b: Term.FunctionTerm if isBlockFunction(b) => b.body :: Nil
      case t: Pkg => t.stats
      // TODO(olafur) would be nice to have an abstract "For" superclass.
      case t: Term.For => getEnumStatements(t.enums)
      case t: Term.ForYield => getEnumStatements(t.enums)
      case t: Term.Match => t.cases
      case t: Type.Match => t.cases
      case t: Term.PartialFunction => t.cases
      case t: Term.Try => t.catchp
      case t: Type.Refine => t.stats
      case t: Source => t.stats
      case t: Template => t.stats
      case t: CaseTree if t.body.tokens.nonEmpty => Seq(t.body)
      case _ => Seq.empty[Tree]
    }

  def getStatementStarts(
      tree: Tree,
      ftoks: FormatTokens,
      soft: SoftKeywordClasses
  ): Map[TokenHash, Tree] = {
    val ret = Map.newBuilder[TokenHash, Tree]
    ret.sizeHint(tree.tokens.length)

    def addFT(ft: FormatToken, tree: Tree): Unit = ret += hash(ft.left) -> tree
    def addTok(token: Token, tree: Tree) = addFT(ftoks.after(token), tree)
    def addTree(t: Tree, o: Tree) = ftoks.getHeadOpt(t).foreach(addFT(_, o))
    def addOne(t: Tree) = addTree(t, t)
    def addAll(trees: Seq[Tree]) = trees.foreach(addOne)

    def addDefnTokens(
        mods: Seq[Mod],
        tree: Tree,
        what: String,
        isMatch: Token => Boolean
    ): Unit = {
      // Each @annotation gets a separate line
      val annotations = mods.filter(_.is[Mod.Annot])
      addAll(annotations)
      mods.find(!_.is[Mod.Annot]) match {
        // Non-annotation modifier, for example `sealed`/`abstract`
        case Some(x) => addTree(x, tree)
        case _ =>
          // No non-annotation modifier exists, fallback to keyword like `object`
          tree.tokens.find(isMatch) match {
            case Some(x) => addTok(x, tree)
            case None => throw Error.CantFindDefnToken(what, tree)
          }
      }
    }
    def addDefn[T](mods: Seq[Mod], tree: Tree)(implicit
        tag: ClassTag[T]
    ): Unit = {
      val runtimeClass = tag.runtimeClass
      addDefnTokens(
        mods,
        tree,
        runtimeClass.getSimpleName,
        runtimeClass.isInstance
      )
    }

    def loop(subtree: Tree): Unit = {
      subtree match {
        case t: Defn.Class => addDefn[KwClass](t.mods, t)
        case t: Decl.Def => addDefn[KwDef](t.mods, t)
        case t: Defn.Def => addDefn[KwDef](t.mods, t)
        case t: Defn.Macro => addDefn[KwDef](t.mods, t)
        case t: Decl.Given => addDefn[KwGiven](t.mods, t)
        case t: Defn.Given => addDefn[KwGiven](t.mods, t)
        case t: Defn.GivenAlias => addDefn[KwGiven](t.mods, t)
        case t: Defn.Enum => addDefn[KwEnum](t.mods, t)
        case t: Defn.ExtensionGroup =>
          addDefnTokens(Nil, t, "extension", soft.KwExtension.unapply)
        case t: Defn.Object => addDefn[KwObject](t.mods, t)
        case t: Defn.Trait => addDefn[KwTrait](t.mods, t)
        case t: Defn.Type => addDefn[KwType](t.mods, t)
        case t: Decl.Type => addDefn[KwType](t.mods, t)
        case t: Defn.Val => addDefn[KwVal](t.mods, t)
        case t: Decl.Val => addDefn[KwVal](t.mods, t)
        case t: Defn.Var => addDefn[KwVar](t.mods, t)
        case t: Decl.Var => addDefn[KwVar](t.mods, t)
        case t: Ctor.Secondary =>
          addDefn[KwDef](t.mods, t)
          addAll(t.stats)
        // special handling for rewritten blocks
        case t @ Term.Block(arg :: Nil) // single-stat block
            if t.tokens.headOption // see if opening brace was removed
              .exists(x => x.is[Token.LeftBrace] && ftoks(x).left.ne(x)) =>
          if (arg.is[Term.Function]) {
            // handle rewritten apply { { x => b } } to a { x => b }
            val parentApply = findTreeWithParent(t) {
              case Term.Block(_) => None
              case p @ Term.ArgClause(_ :: Nil, _)
                  if p.parent.exists(_.is[Term.Apply]) =>
                Some(true)
              case _ => Some(false)
            }
            if (parentApply.isDefined) addOne(arg)
          }
        // special handling for rewritten apply(x => { b }) to a { x => b }
        case t: Term.Apply =>
          val ac = t.argClause
          ac.values match {
            case (f: Term.Function) :: Nil if ac.tokens.lastOption.exists { x =>
                  x.is[Token.RightParen] && // see if closing paren is now brace
                  ftoks.prevNonComment(ftoks(x)).left.is[Token.RightBrace]
                } =>
              addOne(f)
            case _ =>
          }
        case t => // Nothing
          addAll(extractStatementsIfAny(t))
      }
      subtree.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }

  /** Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses(tokens: Iterable[Token]): Map[TokenHash, Token] = {
    val ret = Map.newBuilder[TokenHash, Token]
    var stack = List.empty[Token]
    tokens.foreach {
      case open @ (LeftBrace() | LeftBracket() | LeftParen() |
          Interpolation.Start() | Xml.Start() | Xml.SpliceStart()) =>
        stack = open :: stack
      case close @ (RightBrace() | RightBracket() | RightParen() |
          Interpolation.End() | Xml.End() | Xml.SpliceEnd()) =>
        val open = stack.head
        assertValidParens(open, close)
        ret += hash(open) -> close
        ret += hash(close) -> open
        stack = stack.tail
      case _ =>
    }
    if (stack.nonEmpty)
      throw new IllegalArgumentException(
        stack.map(x => s"[${x.end}]$x").mkString("Orphan parens (", ", ", ")")
      )
    val result = ret.result()
    result
  }

  def assertValidParens(open: Token, close: Token): Unit = {
    (open, close) match {
      case (Interpolation.Start(), Interpolation.End()) =>
      case (Xml.Start(), Xml.End()) =>
      case (Xml.SpliceStart(), Xml.SpliceEnd()) =>
      case (LeftBrace(), RightBrace()) =>
      case (LeftBracket(), RightBracket()) =>
      case (LeftParen(), RightParen()) =>
      case (o, c) =>
        throw new IllegalArgumentException(s"Mismatching parens ($o, $c)")
    }
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
      tree: Tree
  )(pred: Tree => Option[Boolean]): Option[Tree] =
    findTreeEx(tree) { t =>
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
  def findTreeEx(
      tree: Tree
  )(pred: Tree => Option[Tree]): Option[Tree] =
    pred(tree) match {
      case None => None
      case Some(null | `tree`) => Some(tree)
      case Some(r) => findTreeEx(r)(pred)
    }

  def findTreeOrParentSimple(
      tree: Tree,
      flag: Boolean = true
  )(pred: Tree => Boolean): Option[Tree] =
    findTreeOrParent(tree)(x => if (pred(x) == flag) Some(true) else None)

  /** Returns first ancestor whose parent matches the given predicate. The
    * predicate returns None to continue with the parent, or the boolean match
    * flag, which terminates the search.
    */
  def findTreeWithParent(
      tree: Tree
  )(pred: Tree => Option[Boolean]): Option[Tree] =
    findTreeWithParentEx(tree) { t =>
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
      tree: Tree
  )(pred: Tree => Option[Tree]): Option[Tree] =
    tree.parent match {
      case None => None
      case Some(p) =>
        pred(p) match {
          case None => None
          case Some(null) => Some(tree)
          case Some(r) => findTreeWithParentEx(r)(pred)
        }
    }

  def findTreeWithParentSimple(
      tree: Tree,
      flag: Boolean = true
  )(pred: Tree => Boolean): Option[Tree] =
    findTreeWithParent(tree)(x => if (pred(x) == flag) Some(true) else None)

  /** Returns first ancestor with a parent of a given type.
    */
  def findTreeWithParentOfType[A <: Tree](tree: Tree)(implicit
      classifier: Classifier[Tree, A]
  ): Option[Tree] =
    findTreeWithParentSimple(tree)(classifier.apply)

  /** Returns true if a matching ancestor of a given type exists.
    */
  @inline
  def existsParentOfType[A <: Tree](
      tree: Tree
  )(implicit classifier: Classifier[Tree, A]): Boolean =
    findTreeWithParentOfType[A](tree).isDefined

  @tailrec
  def defDefBody(tree: Tree): Option[Tree] =
    tree match {
      case d: Defn with Tree.WithBody => Some(d.body)
      case d: Defn with Stat.WithTemplate => Some(d.templ)
      case t: Ctor.Secondary => Some(t.init)
      case _: Ctor.Primary | _: Pat.Var | _: Term.Name =>
        tree.parent match {
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

  @tailrec
  private def defDefReturnTypeImpl(tree: Tree): Option[Type] =
    tree match {
      case d: Decl.Def => Some(d.decltpe)
      case d: Defn.Def => d.decltpe
      case d: Defn.Macro => d.decltpe
      case d: Defn.Given => d.templ.inits.headOption.map(_.tpe)
      case d: Defn.GivenAlias => Some(d.decltpe)
      case d: Decl.Given => Some(d.decltpe)
      case d: Defn.Val => d.decltpe
      case d: Defn.Var => d.decltpe
      case _: Pat.Var | _: Term.Name | _: Member.ParamClause |
          _: Member.ParamClauseGroup =>
        tree.parent match {
          case Some(p) => defDefReturnTypeImpl(p)
          case _ => None
        }
      case _ => None
    }
  def defDefReturnType(tree: Tree): Option[Type] =
    defDefReturnTypeImpl(tree).filter(!_.pos.isEmpty)

  val DefDefReturnTypeLeft =
    new FormatToken.ExtractFromMeta(x => defDefReturnType(x.leftOwner))
  val DefDefReturnTypeRight =
    new FormatToken.ExtractFromMeta(x => defDefReturnType(x.rightOwner))

  def isParamClauseSite(tree: Tree): Boolean =
    tree match {
      case _: Type.ParamClause => !tree.parent.exists(_.is[Type.Lambda])
      case _: Term.ParamClause =>
        tree.parent match {
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
  def isTokenHeadOrBefore(token: Token, pos: Position): Boolean =
    pos.start >= token.start

  @inline
  def isTokenLastOrAfter(token: Token, owner: Tree): Boolean =
    isTokenLastOrAfter(token, owner.pos)

  @inline
  def isTokenLastOrAfter(token: Token, pos: Position): Boolean =
    pos.end <= token.end

  def isArgClauseSite(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    tree match {
      case t: Member.ArgClause =>
        !t.parent.exists(_.is[Member.Infix]) || (t.values match {
          case (_: Term.Assign | _: Lit.Unit) :: Nil => true
          case Nil | _ :: Nil => false
          case _ => style.newlines.formatInfix
        })
      case _: Term.Super | _: Lit.Unit | _: Term.Tuple | _: Pat.Tuple => true
      case _ => false
    }

  def isTuple(tree: Tree): Boolean =
    tree.is[Member.Tuple]

  def noSpaceBeforeOpeningParen(
      tree: Tree
  ): Boolean =
    tree match {
      case _: Term.Super => true
      case t: Member.ArgClause => !t.parent.exists(_.is[Member.Infix])
      case _: Member.ParamClause =>
        tree.parent.exists {
          case _: Term.FunctionTerm => false
          case t: Ctor.Primary =>
            t.mods.isEmpty || !t.paramClauses.headOption.contains(tree)
          case _ => true
        }
      case _ => false
    }

  def isModPrivateProtected(tree: Tree): Boolean =
    tree match {
      case _: Mod.Private | _: Mod.Protected => true
      case _ => false
    }

  val DefValAssignLeft =
    new FormatToken.ExtractFromMeta(_.leftOwner match {
      case _: Enumerator => None // it's WithBody
      case t: Ctor.Secondary => Some(t.init)
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
    case _ =>
      numParents(tree) {
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

  def maxTreeDepth(trees: Seq[Tree]): Int =
    trees.foldLeft(0) { case (res, t) => math.max(res, treeDepth(t)) }

  @tailrec
  final def lastLambda(first: Term.FunctionTerm): Term.FunctionTerm =
    first.body match {
      case child: Term.FunctionTerm => lastLambda(child)
      case _ => first
    }

  @inline final def isInfixOp(tree: Tree): Boolean =
    AsInfixOp.unapply(tree).isDefined

  object AsInfixOp {
    def unapply(tree: Tree): Option[Member.Infix] =
      tree.parent.collect {
        case ia: Member.Infix if ia.op eq tree => ia
      }
  }

  @inline
  final def asInfixApp(tree: Tree): Option[Member.Infix] =
    InfixApp.unapply(tree)

  @inline
  final def isInfixApp(tree: Tree): Boolean = asInfixApp(tree).isDefined

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
  def isProcedureSyntax(defn: Defn.Def): Boolean =
    defn.decltpe.exists(_.tokens.isEmpty)

  def isXmlBrace(owner: Tree): Boolean =
    owner match {
      case _: Term.Xml | _: Pat.Xml => true
      case b: Term.Block => b.parent.exists(_.isInstanceOf[Term.Xml])
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

  def isSingleElement(elements: List[Tree], value: Tree): Boolean =
    elements match {
      case `value` :: Nil => true
      case _ => false
    }

  def getBlockSingleStat(b: Term.Block): Option[Stat] = b.stats match {
    case stat :: Nil => Some(stat)
    case _ => None
  }

  def isTreeMultiStatBlock(tree: Tree): Boolean = tree match {
    case t: Term.Block => isMultiStatBlock(t)
    case _ => false
  }

  /* An end marker is really more like a closing brace for formatting purposes
   * (but not when rewriting) so we should ignore it when considering whether a
   * block contains only a single statement. NB: in FormatWriter, when choosing
   * to insert or remove end markers, we avoid such borderline cases.
   */
  def getSingleStatExceptEndMarker(s: Seq[Stat]): Option[Stat] =
    s.headOption.filter { _ =>
      val len2 = s.lengthCompare(2)
      len2 < 0 || len2 == 0 && s(1).is[Term.EndMarker]
    }

  def getSingleStatExceptEndMarker(t: Tree): Option[Tree] = t match {
    case Term.Block(s) => getSingleStatExceptEndMarker(s)
    case _ => Some(t)
  }

  def getTreeSingleStat(t: Tree): Option[Tree] =
    t match {
      case b: Term.Block => getBlockSingleStat(b)
      case _ => Some(t)
    }

  def getTreeLineSpan(b: Tree): Int = getTreeLineSpan(b.pos)
  def getTreeLineSpan(pos: Position): Int =
    if (pos.isEmpty) 0 else pos.endLine - pos.startLine

  def hasSingleTermStat(t: Term.Block): Boolean =
    getBlockSingleStat(t).exists(_.is[Term])

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
            rest.exists(noExplicitImplicit) =>
        Some(v)
      case _ => None
    }

  def hasImplicitParamList(kwOwner: Tree): Boolean =
    getImplicitParamList(kwOwner).isDefined

  def shouldNotDangleAtDefnSite(
      tree: Option[Tree],
      isVerticalMultiline: Boolean
  )(implicit style: ScalafmtConfig): Boolean =
    !style.danglingParentheses.defnSite || {
      val excludes = style.danglingParentheses.getExclude(isVerticalMultiline)
      excludes.nonEmpty && tree
        .flatMap {
          case _: Ctor.Primary | _: Defn.Class =>
            Some(DanglingParentheses.Exclude.`class`)
          case _: Defn.Trait => Some(DanglingParentheses.Exclude.`trait`)
          case _: Defn.Enum => Some(DanglingParentheses.Exclude.`enum`)
          case t: Member.ParamClauseGroup =>
            t.parent.collect {
              case _: Defn.ExtensionGroup =>
                DanglingParentheses.Exclude.`extension`
              case _: Decl.Def | _: Defn.Def | _: Defn.Macro =>
                DanglingParentheses.Exclude.`def`
              case _: Decl.Given | _: Defn.Given | _: Defn.GivenAlias =>
                DanglingParentheses.Exclude.`given`
            }
          case _ => None
        }
        .exists(excludes.contains)
    }

  def isChildOfCaseClause(tree: Tree): Boolean =
    findTreeWithParent(tree) {
      case t: Case => Some(tree ne t.body)
      case _: Pat | _: Pat.ArgClause => None
      case _ => Some(false)
    }.isDefined

  object EndOfFirstCall {
    def unapply(tree: Tree): Option[Token] =
      traverse(tree, None).map(_.tokens.last)

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
    if (matches)
      tree.parent match {
        case Some(p) => getLastCall(p, tree)
        case _ => tree
      }
    else lastCall
  }

  @tailrec
  def findInterpolate(tree: Tree): Option[Term.Interpolate] =
    tree match {
      case ti: Term.Interpolate => Some(ti)
      case _ =>
        tree.parent match {
          case Some(p) => findInterpolate(p)
          case _ => None
        }
    }

  def findInterpolateArgAfter(end: Int, tree: Tree): Option[Tree] =
    tree match {
      case t: Pat.Interpolate => findArgAfter(end, t.args)
      case t: Term.Interpolate => findArgAfter(end, t.args)
      case _ => None
    }

  def findArgAfter(end: Int, trees: Seq[Tree]): Option[Tree] =
    trees.find(_.pos.start >= end)

  def getStripMarginCharForInterpolate(tree: Tree): Option[Char] =
    findInterpolate(tree).flatMap(getStripMarginChar)

  def getStripMarginChar(t: Tree): Option[Char] = {
    t.parent match {
      case Some(ts: Term.Select) if ts.name.value == "stripMargin" =>
        ts.parent match {
          case Some(Term.Apply.Initial(_, List(arg: Lit.Char))) =>
            Some(arg.value)
          case _ => Some('|')
        }
      case _ => None
    }
  }

  @inline
  def isTripleQuote(syntax: String): Boolean = syntax.startsWith("\"\"\"")

  @tailrec
  def findFirstTreeBetween(tree: Tree, beg: Token, end: Token): Option[Tree] = {
    def isWithinRange(x: Tokens): Boolean = {
      x.nonEmpty && x.head.start >= beg.start && x.last.end <= end.end
    }
    def matches(tree: Tree): Boolean = {
      val x = tree.tokens
      isWithinRange(x) ||
      x.nonEmpty && x.head.start <= beg.start && x.last.end >= end.end
    }
    if (isWithinRange(tree.tokens)) Some(tree)
    else
      tree.children.find(matches) match {
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
  def existsIfWithoutElse(t: Term.If): Boolean =
    existsIfWithoutElse(t.thenp) || (t.elsep match {
      case x: Term.If => existsIfWithoutElse(x)
      case _ => ifWithoutElse(t)
    })

  def existsIfWithoutElse(tree: Tree): Boolean = tree match {
    case t: Term.If => existsIfWithoutElse(t)
    case _ => false
  }

  def cannotStartSelectChainOnExpr(expr: Term): Boolean =
    expr match {
      case _: Term.Placeholder => true
      case t: Term.Name => isSymbolicName(t.value)
      case t: Term.Select => isSymbolicName(t.name.value)
      case _ => false
    }

  // Redundant {} block around case statements
  def isCaseBodyABlock(ft: FormatToken, caseStat: CaseTree): Boolean =
    ft.right.is[Token.LeftBrace] && (caseStat.body eq ft.meta.rightOwner)

  def getStartOfTemplateBody(template: Template): Option[Token] =
    template.self.tokens.headOption
      .orElse(template.stats.headOption.flatMap(_.tokens.headOption))

  def getTemplateGroups(template: Template): Option[Seq[List[Tree]]] = {
    val groups = Seq(template.inits, template.derives).filter(_.nonEmpty)
    if (groups.isEmpty) None else Some(groups)
  }

  @tailrec
  final def followedBySelectOrApply(tree: Tree): Boolean = tree.parent match {
    case Some(p: Term.New) => followedBySelectOrApply(p)
    case Some(_: Term.Select) => true
    case Some(p: Member.Infix) => p.lhs.eq(tree) || followedBySelectOrApply(p)
    case Some(p: Member.Apply) if p.fun eq tree =>
      p.argClause.values match {
        case (_: Term.Block | _: Term.PartialFunction) :: Nil => false
        case _ => true
      }
    case _ => false
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
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Boolean = {
    def owner = ft.meta.rightOwner
    def isArgOrParamClauseSite(tree: Tree) =
      isArgClauseSite(tree) || isParamClauseSite(tree)
    // skip empty parens/braces/brackets
    ft.right match {
      case _: Token.RightBrace =>
        !left.is[Token.LeftBrace] && owner.is[Importer]
      case _: Token.RightParen =>
        !left.is[Token.LeftParen] && isArgOrParamClauseSite(owner)
      case _: Token.RightBracket =>
        !left.is[Token.LeftBracket] && isArgOrParamClauseSite(owner)
      case _ => false
    }
  }

  def findEnclosedBetweenParens(
      lt: Token,
      rt: Token,
      tree: Tree
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
      baseStyle: ScalafmtConfig
  ): (ScalafmtConfig, collection.Map[TokenHash, Tree]) = {
    var infixCount = 0
    // Creates lookup table from token offset to its closest scala.meta tree
    val ownersMap = HashMap.newBuilder[TokenHash, Tree]
    @inline def setOwner(tok: Token, tree: Tree): Unit =
      ownersMap += hash(tok) -> tree

    val allTokens = topSourceTree.tokens(baseStyle.dialect)
    var prevParens: List[Token] = Nil

    def treeAt(elemIdx: Int, elem: Tree, outerPrevLPs: Int): Int = {
      if (TreeOps.isInfixApp(elem)) infixCount += 1

      val endPos = elem.pos.end
      val allChildren: List[(Tree, Int)] = elem.children
        .flatMap { x =>
          val pos = x.pos
          val startPos = pos.start
          if (startPos == pos.end) None else Some((x, startPos))
        }
        .sortBy(_._2)

      allChildren match {
        case Nil =>
          @tailrec
          def tokenAt(idx: Int): Int =
            if (idx == allTokens.length) idx
            else {
              val tok = allTokens(idx)
              if (elem != topSourceTree && tok.start >= endPos) idx
              else {
                setOwner(tok, elem)
                tokenAt(idx + 1)
              }
            }
          tokenAt(elemIdx)

        case (firstChild, firstChildStart) :: rest =>
          var nextChild = firstChild
          var nextChildStart = firstChildStart
          var children = rest
          var prevChild: Tree = null
          var prevLPs = outerPrevLPs

          @tailrec
          def tokenAt(idx: Int): Int = {
            if (idx == allTokens.length) idx
            else {
              val tok = allTokens(idx)
              val tokStart = tok.start
              if (elem != topSourceTree && tokStart >= endPos) idx
              else if (nextChild != null && tokStart >= nextChildStart) {
                if (prevChild != null) prevLPs = 0
                prevChild = nextChild
                val nextIdx = treeAt(idx, nextChild, prevLPs)
                children match {
                  case Nil =>
                    nextChild = null
                    nextChildStart = endPos
                  case (head, start) :: rest =>
                    nextChild = head
                    children = rest
                    nextChildStart = start
                }
                tokenAt(nextIdx)
              } else {
                def excludeRightParen: Boolean = elem match {
                  case t: Term.If =>
                    prevLPs == 1 && prevChild == t.cond // `expr` after `mods`
                  case _: Term.While | _: Term.For | _: Term.ForYield =>
                    prevLPs == 1 && prevChild == firstChild // `expr` is first
                  case _: Member.SyntaxValuesClause | _: Member.Tuple |
                      _: Term.Do | _: Term.AnonymousFunction =>
                    endPos == tok.end
                  case t: Init => prevChild ne t.tpe // include tpe
                  case _: Ctor.Primary => true
                  case _ => false
                }

                if (prevParens.nonEmpty && tok.is[RightParen]) {
                  if (prevChild == null || prevLPs <= 0 || excludeRightParen)
                    setOwner(tok, elem)
                  else {
                    setOwner(tok, prevChild)
                    setOwner(prevParens.head, prevChild)
                  }
                  prevLPs -= 1
                  prevParens = prevParens.tail
                } else {
                  setOwner(tok, elem)
                  if (!tok.is[Trivia] && tokStart != tok.end) {
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
          }
          tokenAt(elemIdx)
      }
    }

    treeAt(0, topSourceTree, 0)

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

  def isFewerBraces(tree: Term.Apply)(implicit dialect: Dialect): Boolean =
    dialect.allowFewerBraces && tree.argClause.tokens.head.is[Colon]

}
