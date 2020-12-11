package org.scalafmt.util

import java.{util => ju}
import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.Case
import scala.meta.Ctor
import scala.meta.Decl
import scala.meta.Defn
import scala.meta.Enumerator
import scala.meta.Init
import scala.meta.Lit
import scala.meta.Mod
import scala.meta.Name
import scala.meta.Pat
import scala.meta.Pkg
import scala.meta.Stat
import scala.meta.Template
import scala.meta.Term
import scala.meta.Tree
import scala.meta.Type
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens
import scala.reflect.ClassTag
import scala.reflect.classTag

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.Error
import org.scalafmt.config.{DanglingParentheses, ScalafmtConfig}
import org.scalafmt.internal.FormatToken

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

  def withChain(top: Tree): Seq[Type.With] =
    top match {
      case t: Type.With => t +: withChain(t.lhs)
      case _ => Nil
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
  def isBlockFunction(fun: Term.Function): Boolean =
    fun.parent match {
      case Some(b: Term.Block) => isSingleElement(b.stats, fun)
      case Some(next: Term.Function) => isBlockFunction(next)
      case _ => false
    }

  def isFunctionWithBraces(fun: Term.Function): Boolean =
    fun.parent match {
      case Some(b: Term.Block) => isSingleElement(b.stats, fun)
      case _ => false
    }

  def extractStatementsIfAny(tree: Tree): Seq[Tree] =
    tree match {
      case b: Term.Block => b.stats
      case b: Term.Function if isBlockFunction(b) => b.body :: Nil
      case t: Pkg => t.stats
      // TODO(olafur) would be nice to have an abstract "For" superclass.
      case t: Term.For => getEnumStatements(t.enums)
      case t: Term.ForYield => getEnumStatements(t.enums)
      case t: Term.Match => t.cases
      case t: Term.PartialFunction => t.cases
      case t: Term.Try => t.catchp
      case t: Type.Refine => t.stats
      case t: scala.meta.Source => t.stats
      case t: Template => t.stats
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

    def addTok(token: Token, tree: Tree) = ret += hash(token) -> tree
    def addTree(t: Tree, tree: Tree) =
      t.tokens.find(!_.is[Trivia]).foreach(addTok(_, tree))
    def addAll(trees: Seq[Tree]) = trees.foreach(x => addTree(x, x))

    def addDefnTokens[T](
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
            case None =>
              throw Error.CantFindDefnToken(what, tree)
          }
      }
    }
    def addDefn[T: ClassTag](mods: Seq[Mod], tree: Tree): Unit = {
      addDefnTokens[T](
        mods,
        tree,
        classTag[T].runtimeClass.getSimpleName(),
        t => classTag[T].runtimeClass.isInstance(t)
      )
    }

    def loop(x: Tree): Unit = {
      x match {
        case t: Defn.Class => addDefn[KwClass](t.mods, t)
        case t: Defn.Def => addDefn[KwDef](t.mods, t)
        case t: Defn.Macro => addDefn[KwDef](t.mods, t)
        case t: Decl.Def => addDefn[KwDef](t.mods, t)
        case t: Defn.Enum => addDefn[KwEnum](t.mods, t)
        case t: Defn.ExtensionGroup =>
          addDefnTokens(Nil, t, "extension", t => ExtensionKeyword.unapply(t))
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
        case t => // Nothing
          addAll(extractStatementsIfAny(t))
      }
      x.children.foreach(loop)
    }
    loop(tree)
    ret.result()
  }

  /** Finds matching parens [({})].
    *
    * Contains lookup keys in both directions, opening [({ and closing })].
    */
  def getMatchingParentheses(tokens: Tokens): Map[TokenHash, Token] = {
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

  /** Creates lookup table from token offset to its closest scala.meta tree.
    */
  def getOwners(tree: Tree): collection.Map[TokenHash, Tree] = {
    val result = new java.util.HashMap[TokenHash, Tree](2048)
    val workList = new ju.LinkedList[Tree]()
    workList.add(tree)
    while (!workList.isEmpty) {
      val x = workList.poll()
      x.tokens.foreach { tok => result.put(hash(tok), x) }
      workList.addAll(x.children.asJava)
    }
    result.asScala
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
  @tailrec
  def findTreeOrParent(
      tree: Tree
  )(pred: Tree => Option[Boolean]): Option[Tree] =
    pred(tree) match {
      case Some(true) => Some(tree)
      case Some(false) => None
      case None =>
        tree.parent match {
          case None => None
          case Some(p) => findTreeOrParent(p)(pred)
        }
    }
  def findTreeOrParentSimple(
      tree: Tree,
      flag: Boolean = true
  )(pred: Tree => Boolean): Option[Tree] =
    findTreeOrParent(tree)(x => if (pred(x) == flag) Some(true) else None)

  /** Returns first ancestor whose parent matches the given predicate.
    */
  @tailrec
  def findTreeWithParent(
      tree: Tree
  )(pred: Tree => Option[Boolean]): Option[Tree] =
    tree.parent match {
      case None => None
      case Some(p) =>
        pred(p) match {
          case Some(true) => Some(tree)
          case Some(false) => None
          case None => findTreeWithParent(p)(pred)
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

  def isDefDef(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def => true
      case _ => false
    }

  def defDefReturnType(tree: Tree): Option[Type] =
    tree match {
      case d: Decl.Def => Some(d.decltpe)
      case d: Defn.Def => d.decltpe
      case d: Defn.Val => d.decltpe
      case d: Defn.Var => d.decltpe
      case pat: Pat.Var => pat.parent.flatMap(defDefReturnType)
      case name: Term.Name => name.parent.flatMap(defDefReturnType)
      case _ => None
    }

  /** Returns `true` if the `scala.meta.Tree` is a class, trait, enum or def
    *
    * For classes this includes primary and secondary Ctors.
    */
  def isDefnSiteWithParams(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def | _: Defn.Macro | _: Defn.Class |
          _: Defn.Trait | _: Defn.Enum | _: Defn.EnumCase |
          _: Defn.ExtensionGroup | _: Ctor.Secondary =>
        true
      case x: Ctor.Primary =>
        x.parent.exists(isDefnSiteWithParams)
      case _ => false
    }

  /** Returns `true` if the `scala.meta.Tree` is a definition site
    *
    * Currently, this includes everything from classes and defs to type
    * applications
    */
  def isDefnSite(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def | _: Defn.Macro | _: Defn.Class |
          _: Defn.Trait | _: Ctor.Secondary | _: Decl.Type | _: Defn.Type |
          _: Type.Apply | _: Type.Param | _: Type.Tuple | _: Defn.Enum |
          _: Defn.EnumCase | _: Defn.ExtensionGroup =>
        true
      case _: Term.Function | _: Type.Function => true
      case x: Ctor.Primary => x.parent.exists(isDefnSite)
      case _ => false
    }

  /** Returns true if open is "unnecessary".
    *
    * An opening parenthesis is unnecessary if without it and its closing
    * parenthesis can be removed without changing the AST. For example:
    *
    * `(a(1))` will parse into the same tree as `a(1)`.
    */
  def isSuperfluousParenthesis(open: Token, owner: Tree): Boolean =
    open.is[LeftParen] &&
      isSuperfluousParenthesis(open.asInstanceOf[LeftParen], owner)

  def isSuperfluousParenthesis(open: LeftParen, owner: Tree): Boolean =
    !isTuple(owner) && owner.tokens.headOption.contains(open)

  @inline
  def isFirstOrLastToken(token: Token, owner: Tree): Boolean =
    isFirstToken(token, owner) || isLastToken(token, owner)

  @inline
  def isFirstToken(token: Token, owner: Tree): Boolean =
    owner.tokens.headOption.contains(token)

  @inline
  def isLastToken(token: Token, owner: Tree): Boolean =
    owner.tokens.lastOption.contains(token)

  def isCallSite(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    tree match {
      case _: Term.Apply | _: Type.Apply | _: Pat.Extract | _: Term.Super |
          _: Pat.Tuple | _: Term.Tuple | _: Term.ApplyType | _: Term.Assign |
          _: Init | _: Term.ApplyUsing =>
        true
      case t: Term.ApplyInfix => style.newlines.formatInfix && t.args.length > 1
      case _ => false
    }

  def isTuple(tree: Tree): Boolean =
    tree match {
      case _: Pat.Tuple | _: Term.Tuple | _: Type.Tuple => true
      case _ => false
    }

  def isDefnOrCallSite(tree: Tree)(implicit style: ScalafmtConfig): Boolean =
    isDefnSite(tree) || isCallSite(tree)

  def noSpaceBeforeOpeningParen(
      tree: Tree
  )(implicit style: ScalafmtConfig): Boolean =
    !isTuple(tree) && isDefnOrCallSite(tree) && !tree.is[Term.Function]

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

  type CallParts = (Tree, Either[Seq[Tree], Seq[Seq[Tree]]])
  val splitCallIntoParts: PartialFunction[Tree, CallParts] = {
    case t: Term.Apply => (t.fun, Left(t.args))
    case t: Term.Super => (t, Left(Seq(t.superp)))
    case t: Pat.Extract => (t.fun, Left(t.args))
    case t: Pat.Tuple => (t, Left(t.args))
    case t: Type.Apply => (t.tpe, Left(t.args))
    case t: Term.ApplyType => (t.fun, Left(t.targs))
    case t: Term.Tuple => (t, Left(t.args))
    case t: Term.Function => (t, Left(t.params))
    case t: Type.Function => (t, Left(t.params))
    case t: Type.Tuple => (t, Left(t.args))
    case t: Init => (t.tpe, Right(t.argss))
    case t: Term.ApplyUsing => (t.fun, Left(t.args))
  }
  object SplitCallIntoParts {
    def unapply(tree: Tree): Option[CallParts] =
      splitCallIntoParts.lift(tree)
  }

  type DefnParts = (Seq[Mod], Name, Seq[Type.Param], Seq[Seq[Term.Param]])
  val splitDefnIntoParts: PartialFunction[Tree, DefnParts] = {
    // types
    case t: Type.Param => (t.mods, t.name, t.tparams, Seq.empty)
    case t: Decl.Type => (t.mods, t.name, t.tparams, Seq.empty)
    case t: Defn.Type => (t.mods, t.name, t.tparams, Seq.empty)
    // definitions
    case t: Defn.Def => (t.mods, t.name, t.tparams, t.paramss)
    case t: Defn.Macro => (t.mods, t.name, t.tparams, t.paramss)
    case t: Decl.Def => (t.mods, t.name, t.tparams, t.paramss)
    case t: Defn.Class => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.Trait => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.Enum => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.EnumCase => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.ExtensionGroup =>
      (Nil, Name.Anonymous(), t.tparams, List(t.eparam) :: t.uparams)
    case t: Ctor.Primary => (t.mods, t.name, Seq.empty, t.paramss)
    case t: Ctor.Secondary => (t.mods, t.name, Seq.empty, t.paramss)
  }
  object SplitDefnIntoParts {
    def unapply(tree: Tree): Option[DefnParts] =
      splitDefnIntoParts.lift(tree)
  }

  /** How many parents of tree are Term.Apply?
    */
  def nestedApplies(tree: Tree): Int =
    numParents(tree) {
      case _: Term.Apply | _: Term.ApplyInfix | _: Type.Apply => true
      case _ => false
    }

  def nestedSelect(tree: Tree): Int = numParents(tree)(_.is[Term.Select])

  /** Calculates depth to deepest child in tree.
    */
  // TODO(olafur) inefficient, precalculate?

  def treeDepth(tree: Tree): Int =
    if (tree.children.isEmpty) 0
    else 1 + tree.children.map(treeDepth).max

  def defBody(tree: Tree): Option[Tree] =
    tree match {
      case t: Defn.Def => Some(t.body)
      case t: Defn.Macro => Some(t.body)
      case t: Ctor.Secondary => Some(t.init)
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

  final def isInfixOp(tree: Tree): Boolean =
    tree.parent.exists {
      case InfixApp(ia) => ia.op eq tree
      case _ => false
    }

  final def asInfixApp(tree: Tree): Option[InfixApp] = InfixApp.unapply(tree)

  @inline
  final def asInfixApp(tree: Tree, flag: Boolean = true): Option[InfixApp] =
    if (flag) asInfixApp(tree) else None

  @inline
  final def isInfixApp(tree: Tree): Boolean = asInfixApp(tree).isDefined

  @tailrec
  def findNextInfixInParent(tree: Tree, scope: Tree): Option[Name] =
    tree.parent match {
      case Some(t @ InfixApp(ia)) if tree ne scope =>
        if (ia.lhs eq tree) Some(ia.op) else findNextInfixInParent(t, scope)
      case _ => None
    }

  def infixSequenceLength(app: InfixApp): Int = {
    val queue = new mutable.Queue[InfixApp]()
    queue += app
    var length = 0
    while (queue.nonEmpty) {
      val elem = queue.dequeue()
      length += 1
      queue ++= (elem.lhs +: elem.rhs).collect { case InfixApp(ia) =>
        ia
      }
    }
    length
  }

  // procedure syntax has decltpe: Some("")
  def isProcedureSyntax(defn: Defn.Def): Boolean =
    defn.decltpe.exists(_.tokens.isEmpty)

  // matches tree nodes that can be considered "top-level statement": package/object/trait/def/val
  object MaybeTopLevelStat {
    def unapply(tree: Tree): Option[Tree] =
      tree match {
        case _: Pkg | _: Defn | _: Decl => Some(tree)
        case _ => None
      }
  }

  def isXmlBrace(owner: Tree): Boolean =
    owner match {
      case _: Term.Xml | _: Pat.Xml => true
      case b: Term.Block => b.parent.exists(_.isInstanceOf[Term.Xml])
      case _ => false
    }

  def getAssignAtSingleArgCallSite(tree: Tree): Option[Term.Assign] =
    tree match {
      case Term.Apply(_, List(fun: Term.Assign)) => Some(fun)
      case _ => None
    }

  def isSingleElement(elements: List[Tree], value: Tree): Boolean =
    elements.lengthCompare(1) == 0 && (value eq elements.head)

  def getBlockSingleStat(b: Term.Block): Option[Stat] =
    if (b.stats.lengthCompare(1) != 0) None else Some(b.stats.head)

  def getTermSingleStat(t: Term): Option[Tree] =
    t match {
      case b: Term.Block => getBlockSingleStat(b)
      case _ => Some(t)
    }

  def getTermLineSpan(b: Tree): Int =
    if (b.tokens.isEmpty) 0
    else {
      val pos = b.pos
      pos.endLine - pos.startLine
    }

  /** In cases like:
    * {{{
    *   class X(
    *     implicit
    *     private[this] val i1: Int,
    *     private[this] var i2: String
    * )
    * }}}
    *
    * `val i1`, and `var i2` have a ``Mod.Implicit`` with empty tokens.
    */
  def isHiddenImplicit(m: Mod): Boolean =
    m.tokens.isEmpty && m.is[Mod.Implicit]

  def isExplicitImplicit(m: Mod): Boolean =
    m.tokens.nonEmpty && m.is[Mod.Implicit]

  def hasExplicitImplicit(param: Term.Param): Boolean =
    param.mods.exists(isExplicitImplicit)

  def shouldNotDangleAtDefnSite(
      tree: Tree,
      isVerticalMultiline: Boolean
  )(implicit style: ScalafmtConfig): Boolean =
    !style.danglingParentheses.defnSite || {
      val excludeList =
        if (isVerticalMultiline && style.danglingParentheses.exclude.isEmpty)
          style.verticalMultiline.excludeDanglingParens
        else
          style.danglingParentheses.exclude
      excludeList.nonEmpty && {
        val exclude = tree match {
          case _: Ctor.Primary | _: Defn.Class =>
            DanglingParentheses.Exclude.`class`
          case _: Defn.Trait => DanglingParentheses.Exclude.`trait`
          case _: Defn.Enum => DanglingParentheses.Exclude.`enum`
          case _: Defn.ExtensionGroup => DanglingParentheses.Exclude.`extension`
          case _: Defn.Def => DanglingParentheses.Exclude.`def`
          case _ => null
        }
        null != exclude && excludeList.contains(exclude)
      }
    }

  def isChildOfCaseClause(tree: Tree): Boolean =
    findTreeWithParent(tree) {
      case t: Case => Some(tree ne t.body)
      case _: Pat => None
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
        case SplitCallIntoParts(fun, _) if fun ne tree =>
          traverse(fun, Some(fun))
        case _ => res
      }
  }

  @tailrec
  def findInterpolate(
      tree: Tree,
      res: Option[Term.Interpolate] = None
  ): Option[Term.Interpolate] =
    tree.parent match {
      case Some(p: Term.Interpolate) => findInterpolate(p, Some(p))
      case Some(p) => findInterpolate(p, res)
      case _ => res
    }

  def getStripMarginChar(t: Tree): Option[Char] = {
    t.parent match {
      case Some(ts: Term.Select) if ts.name.value == "stripMargin" =>
        ts.parent match {
          case Some(Term.Apply(_, List(arg: Lit.Char))) => Some(arg.value)
          case _ => Some('|')
        }
      case _ => None
    }
  }

  @inline
  def isTripleQuote(syntax: String): Boolean = syntax.startsWith("\"\"\"")

  def getStripMarginChar(ft: FormatToken): Option[Char] = {
    ft.left match {
      case _: Token.Interpolation.Start =>
        val ti = TreeOps.findInterpolate(ft.meta.leftOwner)
        ti.flatMap(TreeOps.getStripMarginChar)
      case _: Token.Constant.String if isTripleQuote(ft.meta.left.text) =>
        TreeOps.getStripMarginChar(ft.meta.leftOwner)
      case _ => None
    }
  }

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

  def cannotStartSelectChain(select: Term.Select): Boolean =
    select.qual match {
      case _: Term.Placeholder => true
      case t: Term.Name => isSymbolicName(t.value)
      case t: Term.Select => isSymbolicName(t.name.value)
      case _ => false
    }

  // Redundant {} block around case statements
  def isCaseBodyABlock(ft: FormatToken, caseStat: Case): Boolean =
    ft.right.is[Token.LeftBrace] && (caseStat.body eq ft.meta.rightOwner)

}
