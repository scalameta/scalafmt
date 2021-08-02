package org.scalafmt.util

import java.{util => ju}
import scala.annotation.tailrec
import scala.collection.immutable.HashMap
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
import scala.meta.CaseTree
import scala.meta.classifiers.Classifier
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens
import scala.reflect.ClassTag

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
      case Some(b: Term.Block) => isSingleElement(b.stats, fun)
      case Some(next: Term.FunctionTerm) => isBlockFunction(next)
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
      case t: scala.meta.Source => t.stats
      case t: Template => t.stats
      case t: CaseTree if t.body.tokens.nonEmpty => Seq(t.body)
      case _ => Seq.empty[Tree]
    }

  def getStatementStarts(
      tree: Tree,
      replacedWith: Token => Token,
      soft: SoftKeywordClasses
  ): Map[TokenHash, Tree] = {
    val ret = Map.newBuilder[TokenHash, Tree]
    ret.sizeHint(tree.tokens.length)

    def addTok(token: Token, tree: Tree) = ret += hash(token) -> tree
    def addTree(t: Tree, tree: Tree) =
      t.tokens.find(!_.is[Trivia]).foreach(addTok(_, tree))
    def addAll(trees: Seq[Tree]) = trees.foreach(x => addTree(x, x))

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
        runtimeClass.getSimpleName(),
        runtimeClass.isInstance
      )
    }

    def loop(x: Tree): Unit = {
      x match {
        case t: Defn.Class => addDefn[KwClass](t.mods, t)
        case t: Defn.Def => addDefn[KwDef](t.mods, t)
        case t: Defn.Given => addDefn[KwGiven](t.mods, t)
        case t: Defn.GivenAlias => addDefn[KwGiven](t.mods, t)
        case t: Defn.Macro => addDefn[KwDef](t.mods, t)
        case t: Decl.Def => addDefn[KwDef](t.mods, t)
        case t: Decl.Given => addDefn[KwGiven](t.mods, t)
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
        case t @ Term.Block(List(_)) // single-stat block
            if t.tokens.headOption // see if opening brace was removed
              .exists(x => x.is[Token.LeftBrace] && replacedWith(x).ne(x)) =>
        // special handling for rewritten apply(x => { b }) to a { x => b }
        case t @ Term.Apply(_, List(f: Term.Function))
            if f.tokens.lastOption // see if closing brace was moved
              .exists(x => x.is[Token.RightBrace] && replacedWith(x).ne(x)) =>
          addAll(Seq(f))
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

  /** Creates lookup table from token offset to its closest scala.meta tree.
    */
  def getOwners(tree: Tree): collection.Map[TokenHash, Tree] = {
    val ownersMap = HashMap.newBuilder[TokenHash, Tree]
    val workList = new ju.LinkedList[Tree]()
    workList.add(tree)
    while (!workList.isEmpty) {
      val x = workList.poll()
      x.tokens.foreach { tok => ownersMap += hash(tok) -> x }
      workList.addAll(x.children.asJava)
    }
    ownersMap.result()
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

  /** Returns first tree which matches the given predicate.
    * The predicate returns None to indicate failure; or the tree to recurse to;
    * if the tree is null (or the same as current tree), the current tree is returned.
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

  /** Returns first ancestor whose parent matches the given predicate.
    * The predicate returns None to continue with the parent, or
    * the boolean match flag, which terminates the search.
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

  /** Returns first ancestor whose parent matches the given predicate.
    * The predicate returns None to indicate failure; or the tree to recurse to;
    * if the recurse-to tree is null, the current tree is returned.
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

  def isDefDef(tree: Tree): Boolean =
    tree match {
      case _: Decl.Def | _: Defn.Def => true
      case _ => false
    }

  @tailrec
  def defDefBody(tree: Tree): Option[Tree] =
    tree match {
      case d: Defn.Def => Some(d.body)
      case d: Defn.Given => Some(d.templ)
      case d: Defn.GivenAlias => Some(d.body)
      case d: Defn.Macro => Some(d.body)
      case d: Defn.Val => Some(d.rhs)
      case d: Defn.Var => d.rhs
      case t: Defn.Class => Some(t.templ)
      case t: Defn.Trait => Some(t.templ)
      case t: Defn.Enum => Some(t.templ)
      case t: Defn.ExtensionGroup => Some(t.body)
      case t: Ctor.Secondary => Some(t.init)
      case _: Ctor.Primary | _: Pat.Var | _: Term.Name =>
        tree.parent match {
          case Some(p) => defDefBody(p)
          case _ => None
        }
      case _ => None
    }

  def defDefReturnType(tree: Tree): Option[Type] =
    tree match {
      case d: Decl.Def => Some(d.decltpe)
      case d: Defn.Def => d.decltpe
      case d: Defn.Given => d.templ.inits.headOption.map(_.tpe)
      case d: Defn.GivenAlias => Some(d.decltpe)
      case d: Decl.Given => Some(d.decltpe)
      case d: Defn.Val => d.decltpe
      case d: Defn.Var => d.decltpe
      case pat: Pat.Var => pat.parent.flatMap(defDefReturnType)
      case name: Term.Name => name.parent.flatMap(defDefReturnType)
      case _ => None
    }
  val DefDefReturnTypeLeft =
    new FormatToken.ExtractFromMeta(x => defDefReturnType(x.leftOwner))
  val DefDefReturnTypeRight =
    new FormatToken.ExtractFromMeta(x => defDefReturnType(x.rightOwner))

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
          _: Defn.EnumCase | _: Defn.ExtensionGroup | _: Decl.Given |
          _: Defn.Given | _: Defn.GivenAlias =>
        true
      case _: Term.FunctionTerm | _: Type.FunctionType => true
      case _: Term.PolyFunction | _: Type.PolyFunction => true
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
    case t: Term.FunctionTerm => (t, Left(t.params))
    case t: Term.PolyFunction => (t, Left(t.tparams))
    case t: Type.FunctionType => (t, Left(t.params))
    case t: Type.PolyFunction => (t, Left(t.tparams))
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
    case t: Defn.Given => (t.mods, t.name, t.tparams, t.sparams)
    case t: Decl.Given => (t.mods, t.name, t.tparams, t.sparams)
    case t: Defn.GivenAlias => (t.mods, t.name, t.tparams, t.sparams)
    case t: Defn.Macro => (t.mods, t.name, t.tparams, t.paramss)
    case t: Decl.Def => (t.mods, t.name, t.tparams, t.paramss)
    case t: Defn.Class => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.Trait => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.Enum => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.EnumCase => (t.mods, t.name, t.tparams, t.ctor.paramss)
    case t: Defn.ExtensionGroup => (Nil, Name.Anonymous(), t.tparams, t.paramss)
    case t: Ctor.Primary => (t.mods, t.name, Seq.empty, t.paramss)
    case t: Ctor.Secondary => (t.mods, t.name, Seq.empty, t.paramss)
  }
  object SplitDefnIntoParts {
    def unapply(tree: Tree): Option[DefnParts] =
      splitDefnIntoParts.lift(tree)
  }

  type AssignParts = (Tree, Option[Seq[Seq[Term.Param]]])
  val splitAssignIntoParts: PartialFunction[Tree, AssignParts] = {
    case t: Defn.Def => (t.body, Some(t.paramss))
    case t: Defn.Macro => (t.body, Some(t.paramss))
    case t: Defn.GivenAlias => (t.body, Some(t.sparams))
    case t: Ctor.Secondary => (t.init, Some(t.paramss))
    case t: Term.Param if t.default.isDefined => (t.default.get, None)
    case t: Term.Assign => (t.rhs, None)
    case t: Defn.Type => (t.body, None)
    case t: Defn.Val => (t.rhs, None)
    case t: Defn.Var => (t.rhs.getOrElse(t), None) // var x: Int = _, no policy
  }
  object SplitAssignIntoParts {
    def unapply(tree: Tree): Option[AssignParts] =
      splitAssignIntoParts.lift(tree)
  }
  val SplitAssignIntoPartsLeft =
    new FormatToken.ExtractFromMeta(x => splitAssignIntoParts.lift(x.leftOwner))

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

  @tailrec
  final def lastLambda(first: Term.FunctionTerm): Term.FunctionTerm =
    first.body match {
      case child: Term.FunctionTerm => lastLambda(child)
      case block: Term.Block
          if block.stats.headOption.exists(_.is[Term.FunctionTerm]) =>
        lastLambda(block.stats.head.asInstanceOf[Term.FunctionTerm])
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

  def isTreeMultiStatBlock(tree: Tree): Boolean = tree match {
    case t: Term.Block => t.stats.lengthCompare(1) > 0
    case _ => false
  }

  def getTreeSingleStat(t: Tree): Option[Tree] =
    t match {
      case b: Term.Block => getBlockSingleStat(b)
      case _ => Some(t)
    }

  def getTreeLineSpan(b: Tree): Int =
    if (b.tokens.isEmpty) 0
    else {
      val pos = b.pos
      pos.endLine - pos.startLine
    }

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
          case _: Defn.Given => DanglingParentheses.Exclude.`given`
          case _: Defn.GivenAlias => DanglingParentheses.Exclude.`given`
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

  @inline
  def getLastCall(tree: Tree): Tree = getLastCall(tree, tree)

  @tailrec
  private def getLastCall(tree: Tree, lastCall: Tree): Tree = {
    // this is to cover types which include one parameter group at a time
    def body = tree match {
      case t: Term.Apply => t.fun
      case t: Pat.Extract => t.fun
      case t: Term.ApplyType => t.fun
      case t: Term.ApplyUsing => t.fun
      case _ => tree
    }
    if (lastCall.eq(tree) || lastCall.eq(body))
      tree.parent match {
        case Some(p) => getLastCall(p, tree)
        case _ => tree
      }
    else lastCall
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

  def isFirstInit(t: Template, owner: Tree) =
    t.inits.headOption.exists { init =>
      // [init.tpe == leftOwner] part is about expressions like [new A with B]
      // [leftOwner.is[Init] && init == leftOwner] part is about expressions like [new A(x) with B]
      owner.is[Init] && init == owner || init.tpe == owner
    }

  def getStartOfTemplateBody(template: Template): Option[Token] =
    template.self.tokens.headOption
      .orElse(template.stats.headOption.flatMap(_.tokens.headOption))

  def getAndOrTypeRhs(tree: Tree): Option[Type] = tree match {
    case x: Type.Or => Some(x.rhs)
    case x: Type.And => Some(x.rhs)
    case _ => None
  }

  @tailrec
  final def getTopAndOrType(tree: Tree): Tree = tree.parent match {
    case Some(x @ (_: Type.Or | _: Type.And)) => getTopAndOrType(x)
    case _ => tree
  }

  def getTemplateGroups(template: Template): Option[Seq[List[Tree]]] = {
    val groups = Seq(template.inits, template.derives).filter(_.nonEmpty)
    if (groups.isEmpty) None else Some(groups)
  }

}
