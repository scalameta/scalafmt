package org.scalafmt.rewrite

import org.scalafmt.config.GroupImportsSettings

import scala.meta.{
  Tree,
  Importer => MetaImporter,
  Importee,
  Token,
  Import => MetaImport
}
import scala.meta.tokens.Token.LF
import scala.meta.tokens.Token.Space
import scala.meta.tokens.Token.Tab
import scala.meta.tokens.Token.CR
import scala.meta.tokens.Token.Comment
import scala.meta.tokens.Tokens

case object GroupImports extends Rewrite {

  @inline private def settings(
      implicit ctx: RewriteCtx
  ): GroupImportsSettings =
    ctx.style.rewrite.groupImports

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {
    implicit def _ctx = ctx

    import ctx.dialect
    val groupedImports =
      code.collect { case i: MetaImport => i }.groupBy(_.parent)

    groupedImports.flatMap {
      case (parent, imports) => sortImports(parent.get, imports)
    }.toSeq
  }

  private def sortImports(parent: Tree, imports: List[MetaImport])(
      implicit ctx: RewriteCtx
  ): Seq[Patch] = {
    val firstToken = imports.head.tokens.head

    // comments in the end of line should be deleted separately
    val (imps, comments) = imports.flatMap { i =>
      val lastComment = findComments(parent, i)
      val comments = i.tokens.collect { case c: Comment => c }

      val commentByImporter = i.importers
        .sliding(2)
        .map { pair =>
          pair.head -> comments.filter { c =>
            c.start >= pair.head.pos.end && c.end <= pair.last.pos.start
          }
        }
        .toMap
        .withDefaultValue(Seq.empty) + (i.importers.last -> lastComment)

      i.importers.map { im =>
        (Import(i, Importer(im, commentByImporter(im))), lastComment)
      }
    }.unzip

    val deleteImports =
      imps.map(_.orig).distinct.flatMap(deleteImport(parent, _))
    val deleteComments = comments.flatMap(_.map(TokenPatch.Remove))

    val sorted = imps.sorted(ImportOrdering).toSeq
    val grouped = group(sorted)

    val lineBetween = if (settings.emptyLineBetweenGroups) "\n\n" else "\n"

    deleteImports ++ deleteComments :+ TokenPatch.AddLeft(
      firstToken,
      grouped
        .map(_.map(_.toString()).mkString("\n"))
        .mkString(lineBetween) + "\n"
    )
  }

  /**
    * Groups imports according settings
    *
    * @param col (<import text>, <import ref>, <the first ident>) sequence
    */
  private def group(
      col: Seq[Import]
  )(implicit ctx: RewriteCtx): Seq[Seq[Import]] = {
    val groups = settings.groups

    groups
      .map {
        case g if g == "_" =>
          col.filter { i =>
            !groups.contains(i.importer.first)
          }
        case g =>
          col.filter { i =>
            i.importer.first.toString == g
          }
      }
      .filter(_.nonEmpty)
  }

  /**
    * Deletes import and all "space" symbols between it and the next identifier
    */
  private def deleteImport(parent: Tree, `import`: MetaImport): Seq[Patch] = {
    val spaces = parent.tokens
      .dropWhile(t => t.isNot[LF] || t.pos.startLine != `import`.pos.endLine)
      .takeWhile { t =>
        t.is[LF] || t.is[Space] || t.is[Tab] || t.is[CR]
      }

    `import`.tokens.map(TokenPatch.Remove) ++ spaces.map(TokenPatch.Remove)
  }

  private def findComments(parent: Tree, `import`: MetaImport): Seq[Comment] =
    parent.tokens
      .collect { case c: Comment => c }
      .filter(
        c =>
          c.pos.startLine == `import`.pos.endLine && c.pos.start >= `import`.pos.end
      )

  private case class Import(
      orig: MetaImport,
      importer: Importer
  ) {
    override def toString(): String =
      s"import ${importer.toString()}"
  }

  private case class Importer(
      orig: MetaImporter,
      comments: Seq[Comment] = List.empty
  ) {
    def first: String = orig.tokens.head.toString()

    override def toString(): String = {
      val commentsStr =
        if (comments.nonEmpty) " " + comments.mkString(" ") else ""
      orig.toString() + commentsStr
    }
  }

  private object ImportOrdering extends Ordering[Import] {
    def compare(a: Import, b: Import) =
      a.importer.orig.ref.toString compare b.importer.orig.ref.toString
  }
}
