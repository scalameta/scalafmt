package org.scalafmt

import org.scalafmt.internal._
import org.scalafmt.util.LoggerOps

import scala.meta.Tree
import scala.meta.inputs.Position
import scala.meta.internal.inputs._

import scala.reflect.ClassTag
import scala.reflect.classTag
import scala.util.control.NoStackTrace

sealed abstract class Error(msg: String) extends Exception(msg)

object Error {
  import LoggerOps._

  val cfgUrl = "https://scalameta.org/scalafmt/docs/configuration.html"

  case class Incomplete(formattedCode: String)
      extends Error("Unable to format file due to bug in scalafmt")

  case class PreciseIncomplete(pos: Position, formattedCode: String)
      extends Error(
        pos
          .formatMessage("error", "Unable to format file due to bug in scalafmt"),
      )
  case class CantFindDefnToken(what: String, tree: Tree)
      extends Error(s"Expected keyword of type $what in tree $tree")

  case class FormatterChangedAST(diff: String, output: String)
      extends Error(
        s"""|Formatter changed AST
            |=====================
            |$diff
            |=====================
            |${output.linesIterator.toVector.take(10).mkString("\n")}
            |=====================
            |Formatter changed AST
            |""".stripMargin,
      )

  case class FormatterOutputDoesNotParse(msg: String, line: Int)
      extends Error("Formatter output does not parse:\n" + msg)

  case class UnexpectedTree[Expected <: Tree: ClassTag](obtained: Tree)
      extends Error(
        s"""|Expected: ${classTag[Expected].runtimeClass.getName}
            |Obtained: ${log(obtained)}""".stripMargin,
      )

  case class SearchStateExploded private (
      deepestState: State,
      partialOutput: String,
      tok: String,
      line: Int,
      why: String,
  ) extends Error(
        s"Search state exploded on '$tok', line $line: $why [see $cfgUrl#search-state-exploded]",
      ) {
    def this(deepestState: State, ft: FT, why: String)(implicit
        formatWriter: FormatWriter,
    ) = this(
      deepestState,
      formatWriter.mkString(deepestState),
      LoggerOps.log2(ft),
      ft.left.pos.endLine + 1,
      why,
    )
  }

  case object NoMatchingFiles
      extends Error(
        "No files formatted/tested. " +
          "Verify include/exclude filters and command line arguments.",
      )
      with NoStackTrace

  case class IdempotencyViolated(msg: String) extends Error(msg)

  case class WithCode(error: Throwable, code: String)
      extends Exception(error.getMessage, error)

}
