package org.scalafmt
package internal

import org.scalafmt.Error
import org.scalafmt.util._

import scala.meta.tokens.{Token => T}
import scala.meta.{Token => _, _}

import scala.collection.mutable
import scala.reflect.ClassTag

class OptimizationEntities(
    argumentStarts: Array[Tree],
    optionalNewlines: Set[Int],
    // indexed by token idx (null = absent); array, not boxed-Int Map, since
    // `isStatementStart` is read per NL state in the search
    statementStarts: Array[Tree],
    val semicolons: Map[Int, FT],
) {
  def argumentAt(idx: Int): Tree =
    if (idx >= 0 && idx < argumentStarts.length) argumentStarts(idx) else null
  def argument(implicit ft: FT): Tree = argumentAt(ft.meta.idx)
  def optionalNL(implicit ft: FT): Boolean = optionalNewlines(ft.meta.idx)
  def semicolonAfterStatement(idx: Int): Option[FT] = semicolons.get(idx)
  def statementStart(idx: Int): Tree =
    if (idx >= 0 && idx < statementStarts.length) statementStarts(idx) else null
  def isStatementStart(idx: Int): Boolean = statementStart(idx) ne null
}

object OptimizationEntities {
  def apply(tree: Tree)(implicit
      ftoks: FormatTokens,
      soft: SoftKeywordClasses,
  ): OptimizationEntities = new Builder(tree).build()

  private class Builder(topSourceTree: Tree)(implicit
      ftoks: FormatTokens,
      soft: SoftKeywordClasses,
  ) {

    private val arguments = new Array[Tree](ftoks.length)
    private val optional = Set.newBuilder[Int]
    private val statements = new Array[Tree](ftoks.length)
    private val semicolons = Map.newBuilder[Int, FT]

    def build(): OptimizationEntities = {
      val queue = new mutable.ListBuffer[Tree]
      queue += topSourceTree
      while (queue.nonEmpty) {
        val tree = queue.remove(0)
        processForArguments(tree)
        processForStatements(tree)
        tree.foreachChild(queue += _)
      }
      new OptimizationEntities(
        arguments,
        optional.result(),
        statements,
        semicolons.result(),
      )
    }

    private def getHeadIndex(tree: Tree): Int = ftoks.getHead(tree)
      .nnFold(-1)(_.meta.idx - 1)
    private def addArgWith(key: Tree)(value: Tree): Unit = {
      val idx = getHeadIndex(key)
      if (idx >= 0 && (arguments(idx) eq null)) arguments(idx) = value
    }
    private def addArg(tree: Tree): Unit = addArgWith(tree)(tree)
    private def addOptional(tree: Tree): Unit = {
      val idx = getHeadIndex(tree)
      if (idx >= 0) optional += idx
    }
    private def addParam(t: Term.Param, key: Tree): Unit = {
      addArgWith(key)(t)
      t.mods.foreach(addOptional)
      addOptional(t.name)
    }

    private def processForArguments(tree: Tree): Unit = tree match {
      case _: Lit.Unit =>
      case t: Term.ParamClause =>
        val params = t.mod match {
          case Some(mod) =>
            addOptional(mod)
            t.values match {
              case head :: rest =>
                addParam(head, mod)
                rest
              case _ => Nil
            }
          case _ => t.values
        }
        params.foreach(x => addParam(x, x))
      case t: Term.ArgClause => addArg(t)
      case t: Member.SyntaxValuesClause => t.values.foreach(addArg)
      case t: Member.Tuple => t.args.foreach(addArg)
      case _: Term.Param => // covered by Term.ParamClause
      case t: Term => addArg(t)
      case _ =>
    }

    private def addStmtFT(stmt: Tree, prev: FT = null)(ft: FT): Unit = {
      if (stmt ne null) {
        val isComment = ft.left.is[T.Comment]
        val nft = if (isComment) ftoks.nextAfterNonComment(ft) else ft
        statements(nft.meta.idx) = stmt
      }
      if (prev ne null) {
        val pft = ftoks.prevNonCommentBefore(ft)
        if (pft.left.is[T.Semicolon]) semicolons +=
          prev.idx -> ftoks.nextNonCommentSameLine(pft)
      }
    }
    private def addStmtTok(stmt: Tree)(token: T) =
      addStmtFT(stmt)(ftoks.after(token))
    private def addStmtTree(t: Tree, stmt: Tree, prev: FT = null) = {
      val ft = ftoks.getHead(t)
      if (ft ne null) addStmtFT(stmt, prev)(ft)
      ft
    }
    private def addOneStmt(t: Tree, prev: FT = null) = addStmtTree(t, t, prev)
    private def addAllStmts(trees: Iterable[Tree]) = trees
      .foldLeft(null: FT)((prev, t) => addOneStmt(t, prev))

    private def addDefnTokens(
        mods: Seq[Mod],
        tree: Tree,
        what: String,
        isMatch: T => Boolean,
    ): Unit = {
      addAllStmts(mods.view.filter(_.is[Mod.Annot])) // Each @annotation gets a separate line
      mods.findOrNull(!_.is[Mod.Annot]).nnFold(
        tree.tokens.find(isMatch) // No non-annotation modifier, fallback to keyword like `object`
          .fold(throw Error.CantFindDefnToken(what, tree))(addStmtTok(tree)),
      )(x => addStmtTree(x, tree)) // Non-annotation modifier, for example `sealed`/`abstract`

    }

    private def addDefn[T](mods: Seq[Mod], tree: Tree)(implicit
        tag: ClassTag[T],
    ): Unit = {
      val runtimeClass = tag.runtimeClass
      addDefnTokens(
        mods,
        tree,
        runtimeClass.getSimpleName,
        runtimeClass.isInstance,
      )
    }

    private def processForStatements(tree: Tree): Unit = tree match {
      case t: Defn.Class => addDefn[T.KwClass](t.mods, t)
      case t: Decl.Def => addDefn[T.KwDef](t.mods, t)
      case t: Defn.Def => addDefn[T.KwDef](t.mods, t)
      case t: Defn.Macro => addDefn[T.KwDef](t.mods, t)
      case t: meta.Stat.GivenLike => addDefn[T.KwGiven](t.mods, t)
      case t: Defn.Enum => addDefn[T.KwEnum](t.mods, t)
      case t: Defn.EnumCase => addDefn[T.KwCase](t.mods, t)
      case t: Defn.ExtensionGroup =>
        addDefnTokens(Nil, t, "extension", soft.KwExtension.unapply)
      case t: Defn.Object => addDefn[T.KwObject](t.mods, t)
      case t: Defn.Trait => addDefn[T.KwTrait](t.mods, t)
      case t: Defn.Type => addDefn[T.KwType](t.mods, t)
      case t: Decl.Type => addDefn[T.KwType](t.mods, t)
      case t: Defn.Val => addDefn[T.KwVal](t.mods, t)
      case t: Decl.Val => addDefn[T.KwVal](t.mods, t)
      case t: Defn.Var => addDefn[T.KwVar](t.mods, t)
      case t: Decl.Var => addDefn[T.KwVar](t.mods, t)
      case t: Ctor.Secondary =>
        addDefn[T.KwDef](t.mods, t)
        addAllStmts(t.body.stats)
      // special handling for rewritten blocks
      case t @ Term.Block(_ :: Nil)
          if t.tokens.headOption.exists(x =>
            // ignore single-stat block if opening brace was removed
            x.is[T.LeftBrace] && ftoks(x).left.ne(x),
          ) =>
      case t: Term.EnumeratorsBlock =>
        var wasGuard = false
        def iter(prev: FT, curr: Enumerator): FT = {
          val isGuard = curr.is[Enumerator.Guard]
          // Only guard that follows another guard starts a statement.
          val ok = wasGuard || !isGuard
          wasGuard = isGuard
          addStmtTree(curr, if (ok) curr else null, prev)
        }
        t.enums match {
          case head :: tail => tail.foldLeft(ftoks.getHead(head))(iter)
          case _ =>
        }
      case t: Term.PartialFunction => t.cases match {
          case _ :: Nil =>
          case x => addAllStmts(x)
        }
      case t @ Term.Block(s) =>
        if (t.parent.is[CaseTree]) addAllStmts(
          if (TreeOps.getSingleStatExceptEndMarker(s) eq null) s else s.drop(1),
        )
        else s match {
          case x :: Nil
              if skipBlockSingleStat(x, t.parent.is[Term.ArgClause]) =>
          case _ => addAllStmts(s)
        }
      // handle argclause rewritten with braces
      case t @ Term.ArgClause(s :: Nil, _)
          if !s.isAny[Term.Block, Term.PartialFunction] &&
            !skipBlockSingleStat(s, isInArgClause = true) =>
        t.tokens.headOption.foreach(x =>
          // check for single-stat arg if opening paren was replaced with brace
          if (x.is[T.LeftParen] && ftoks(x).left.is[T.LeftBrace]) addOneStmt(s),
        )
      case Tree.Block(s) => addAllStmts(s)
      case _ => // Nothing
    }

  }

  private def skipBlockSingleStat(t: Tree, isInArgClause: => Boolean): Boolean =
    t match {
      case _: Term.FunctionLike => true
      case _ if !isInArgClause => false
      case _: Term.Apply => true
      case x: Term.AnonymousFunction => x.body.is[Term.Apply]
      case _ => false
    }

}
