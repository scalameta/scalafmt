package org.scalafmt.rewrite

import scala.util.matching.Regex
import scala.meta._
import scala.meta.tokens.Token._

object SortImportStatements extends Rewrite {

  import TokenPatch._
  import Matcher._

  private val patterns: List[String] =
    List("java", "scala", "*", "com.example")

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {

    import ctx.dialect
    val base: (Seq[Importer], Seq[Remove]) =
      (Seq.empty, Seq.empty)
    val (decopImps, rms) =
      code match {
        case Pkg(_, stats) =>
          stats.takeWhile(_.is[Import]).foldLeft(base) {
            case ((acc, racc), stat @ Import(imports)) =>
              (acc ++ imports, racc ++ stat.tokens.map(Remove(_)))
            case (a, _) => a
          }
      }
    if (decopImps.isEmpty)
      Nil
    else {
      def startsWith(imp: Importer, p: String) = imp.toString.startsWith(p)
      val sorted: Seq[Seq[Importer]] = decopImps
        .map { imp =>
          val group = patterns.zipWithIndex.foldLeft(None: Option[Matcher]) {
            case (None, ("*", i)) => Some(GeneralFind(i))
            case (None, (p, i)) if (startsWith(imp, p)) => Some(ExactFind(i))
            case (Some(GeneralFind(_)), (p, i)) if (startsWith(imp, p)) =>
              Some(ExactFind(i))
            case (acc, _) => acc
          }
          // DESNOTE(2017-09-06, pjrt): If we don't find any pattern that
          // matches, just place it at the end. Only way this can happen is
          // if the user doesn't place a `*` anywhere in the pattern list.
          imp -> group.fold(patterns.length)(_.index)
        }
        .groupBy(_._2)
        .map { case (i, imps) => i -> imps.map(_._1) }
        .toSeq
        .sortBy(_._1)
        .map(_._2)

      val sortedStr =
        sorted
          .map(is => is.map(i => s"import $i").sorted.mkString("\n"))
          .mkString("\n\n")
      rms :+ AddRight(rms.head.tok, sortedStr)
    }
  }
}

private sealed trait Matcher {

  def index: Int
}

private object Matcher {

  final case class ExactFind(index: Int) extends Matcher
  final case class GeneralFind(index: Int) extends Matcher
}
