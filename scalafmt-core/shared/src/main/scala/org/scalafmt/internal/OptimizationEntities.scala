package org.scalafmt.internal

import org.scalafmt.Error
import org.scalafmt.util._

import scala.meta.tokens.{Token => T}
import scala.meta.{Token => _, _}

import scala.collection.mutable
import scala.reflect.ClassTag

class OptimizationEntities(
    argumentStarts: Map[Int, Tree],
    optionalNewlines: Set[Int],
    val statementStarts: Map[Int, Tree],
    val semicolons: Map[Int, FT],
) {
  def argumentAt(idx: Int): Option[Tree] = argumentStarts.get(idx)
  def argument(implicit ft: FT): Option[Tree] = argumentAt(ft.meta.idx)
  def optionalNL(implicit ft: FT): Boolean = optionalNewlines(ft.meta.idx)
  def semicolonAfterStatement(idx: Int): Option[FT] = semicolons.get(idx)
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

    private val arguments = mutable.Map.empty[Int, Tree]
    private val optional = Set.newBuilder[Int]
    private val statements = Map.newBuilder[Int, Tree]
    private val semicolons = Map.newBuilder[Int, FT]

    def build(): OptimizationEntities = {
      val queue = new mutable.ListBuffer[Seq[Tree]]
      queue += topSourceTree :: Nil
      while (queue.nonEmpty) queue.remove(0).foreach { tree =>
        processForArguments(tree)
        processForStatements(tree)
        queue += tree.children
      }
      new OptimizationEntities(
        arguments.toMap,
        optional.result(),
        statements.result(),
        semicolons.result(),
      )
    }

    private def getHeadIndex(tree: Tree): Option[Int] = ftoks.getHeadOpt(tree)
      .map(_.meta.idx - 1)
    private def addArgWith(key: Tree)(value: Tree): Unit = getHeadIndex(key)
      .foreach(arguments.getOrElseUpdate(_, value))
    private def addArg(tree: Tree): Unit = addArgWith(tree)(tree)
    private def addOptional(tree: Tree): Unit = getHeadIndex(tree)
      .foreach(optional += _)
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

    private def addStmtFT(stmt: Tree, prev: Option[FT] = None)(ft: FT): Unit = {
      if (stmt ne null) {
        val isComment = ft.left.is[T.Comment]
        val nft = if (isComment) ftoks.nextAfterNonComment(ft) else ft
        statements += nft.meta.idx -> stmt
      }
      prev.foreach { prev =>
        val pft = ftoks.prevNonCommentBefore(ft)
        if (pft.left.is[T.Semicolon]) semicolons +=
          prev.idx -> ftoks.nextNonCommentSameLine(pft)
      }
    }
    private def addStmtTok(stmt: Tree)(token: T) =
      addStmtFT(stmt)(ftoks.after(token))
    private def addStmtTree(t: Tree, stmt: Tree, prev: Option[FT] = None) = {
      val ft = ftoks.getHeadOpt(t)
      ft.foreach(addStmtFT(stmt, prev))
      ft
    }
    private def addOneStmt(t: Tree, prev: Option[FT] = None) =
      addStmtTree(t, t, prev)
    private def addAllStmts(trees: Seq[Tree]) = trees
      .foldLeft(Option.empty[FT])((prev, t) => addOneStmt(t, prev))

    private def addDefnTokens(
        mods: Seq[Mod],
        tree: Tree,
        what: String,
        isMatch: T => Boolean,
    ): Unit = {
      // Each @annotation gets a separate line
      val annotations = mods.filter(_.is[Mod.Annot])
      addAllStmts(annotations)
      mods.find(!_.is[Mod.Annot]) match {
        // Non-annotation modifier, for example `sealed`/`abstract`
        case Some(x) => addStmtTree(x, tree)
        case _ =>
          // No non-annotation modifier exists, fallback to keyword like `object`
          tree.tokens.find(isMatch)
            .fold(throw Error.CantFindDefnToken(what, tree))(addStmtTok(tree))
      }
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
        def iter(prev: Option[FT], curr: Enumerator): Option[FT] = {
          val isGuard = curr.is[Enumerator.Guard]
          // Only guard that follows another guard starts a statement.
          val ok = wasGuard || !isGuard
          wasGuard = isGuard
          addStmtTree(curr, if (ok) curr else null, prev)
        }
        t.enums match {
          case head :: tail => tail.foldLeft(ftoks.getHeadOpt(head))(iter)
          case _ =>
        }
      case t: Term.PartialFunction => t.cases match {
          case _ :: Nil =>
          case x => addAllStmts(x)
        }
      case t @ Term.Block(s) =>
        if (t.parent.is[CaseTree]) addAllStmts(
          if (TreeOps.getSingleStatExceptEndMarker(s).isEmpty) s else s.drop(1),
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
