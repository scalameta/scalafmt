package org.scalafmt.rewrite

import scala.util.matching.Regex
import scala.meta._

object SortImportStatements extends Rewrite {

  import TokenPatch._
  import Matcher._

  private val patterns: List[String] =
    List("java", "scala", "*", "com.example")

  override def rewrite(code: Tree, ctx: RewriteCtx): Seq[Patch] = {

    import ctx.dialect
    val base: (Seq[Importer], Seq[Remove], Option[Token]) =
      (Seq.empty, Seq.empty, None)
    val (decopImps, rms, fstOpt) = code.children.foldLeft(base) {
      case ((acc, racc, None), stat @ Import(imports)) =>
        (
          acc ++ imports,
          racc ++ stat.tokens.map(Remove(_)),
          Some(stat.tokens.head))
      case ((acc, racc, fst), stat @ Import(imports)) =>
        (acc ++ imports, racc ++ stat.tokens.map(Remove(_)), fst)
      case (a, _) => a
    }
    def startsWith(imp: Importer, p: String) =
      imp.toString.startsWith(p)
    val sorted: Seq[Importer] = decopImps
      .map { imp =>
        val group = patterns.zipWithIndex.foldLeft(None: Option[Matcher]) {
          case (None, (p, i)) =>
            if (p == "*")
              Some(GeneralFind(i))
            else if (startsWith(imp, p)) {
              Some(ExactFind(i))
            } else {
              None
            }
          case (keep @ Some(GeneralFind(_)), (p, i)) =>
            if (startsWith(imp, p)) Some(ExactFind(i))
            else keep
          case (acc, _) => acc

        }
        imp -> group.fold(patterns.length)(_.index)
      }
      .groupBy(_._2)
      .map { case (i, imps) => i -> imps.map(_._1) }
      .toSeq
      .sortBy(_._1)
      .flatMap(_._2)

    val sortedStr = sorted.map(i => s"import $i").mkString("\n")
    rms :+ AddRight(fstOpt.get, sortedStr)
  }
}

private sealed trait Matcher {

  def index: Int
}

private object Matcher {

  final case class ExactFind(index: Int) extends Matcher
  final case class GeneralFind(index: Int) extends Matcher
}
