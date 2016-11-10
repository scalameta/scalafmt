package org.scalafmt.diff

import scala.meta.Tree
import scala.meta.tokens.Token.RightBrace
import scala.util.Try
import scala.util.matching.Regex

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatOps
import org.scalafmt.internal.FormatToken
import org.scalafmt.util.TokenOps
import org.scalafmt.util.logger

case class FormatTokenRange(start: FormatToken, end: FormatToken) {
  def contains(tok: FormatToken): Boolean = {
    val result = tok.left.start >= start.left.start && tok.right.end <= end.right.end
    logger.elem(tok, result)
    result
  }
  override def toString: String = s"$start <-> $end"
}
case class Addition(startLine: Int, lineCount: Int) {
  def endLine: Int = startLine + lineCount - 1
  def toRange: Range = Range(startLine, endLine)
}
case class FileDiff(filename: String, additions: Seq[Addition])

object FileDiff {
  val NewFile: Regex = "^\\+\\+\\+\\ (.*?/)(\\S*)".r
  val DiffBlock: Regex =
    "^@@.*\\+(\\d+)(,(\\d+))?".r("startLine", "skip", "lineCount")

  /** Parses a unified diff into FileDiffs.
    *
    * Example commands to produce unified diff:
    *    git diff -U0 HEAD
    *    svn diff --diff-cmd=diff -x-U0
    *
    * Produces something like:
    *    --- /dev/null
    *    +++ b/DiffTest.scala
    *    @@ -54 +54,2 @@ class Router(formatOps: FormatOps) {
    *
    * Example parsed result:
    *    Seq(FileDiff("DiffTest.scala", Seq(Addition(54, 2))))
    */
  def fromUnified(diff: String): Seq[FileDiff] = {
    var currentFilename = Option.empty[String]
    val fileDiffs = Seq.newBuilder[FileDiff]
    val additions = Seq.newBuilder[Addition]
    def addLastFile(): Unit = {
      currentFilename.foreach { lastFilename =>
        fileDiffs += FileDiff(lastFilename, additions.result())
        additions.clear()
      }
    }
    diff.lines.foreach {
      case NewFile(_, filename) =>
        addLastFile()
        currentFilename = Some(filename)
      case other =>
        for {
          diffBlock <- DiffBlock.findAllMatchIn(other)
          startLine <- Try(diffBlock.group("startLine").toInt).toOption.toIterable
          lineCount = Try(diffBlock.group("lineCount").toInt).getOrElse(1)
        } {
          additions += Addition(startLine, lineCount)
        }
    }
    addLastFile()
    fileDiffs.result()
  }

  /** Returns the start and end FormatTokens corresponding to each addition. */
  def getFormatTokenRanges(tokens: Array[FormatToken],
                           additions: Seq[Range]): Seq[FormatTokenRange] = {
    val builder = Seq.newBuilder[FormatTokenRange]
    val N = tokens.length
    var curr = 0
    def getLine(tok: FormatToken): Int = tok.right.pos.start.line + 1
    def forwardToLine(line: Int): Unit = {
      while (curr < N && getLine(tokens(curr)) < line) {
        curr += 1
      }
      if (curr >= N) curr = N - 1 // edge case, EOF
    }
    additions.foreach { addition =>
      forwardToLine(addition.start)
      val start = curr
      curr -= 1 // end can be same as start in case of multi-line token.
      forwardToLine(addition.end + 1)
      val end = curr
      builder += FormatTokenRange(tokens(start), tokens(end))
    }
    builder.result()
  }

  def expandToEnclosingStatements(formatTokenRange: FormatTokenRange,
                                  formatOps: FormatOps): FormatTokenRange = {
    val start =
      formatOps
        .findFirstRev(formatTokenRange.start, formatOps.tokens.head.left) {
          token =>
            // include the newline before the beginning of this statement.
            formatOps.dequeueSpots.contains(TokenOps.hash(token.right))
        }
        .getOrElse(formatTokenRange.start)
    val end =
      formatOps
        .findFirst(formatTokenRange.end, formatOps.tokens.last.right) {
          token =>
            // exclude the newline before the beginning of the next statement.
            formatOps.dequeueSpots.contains(TokenOps.hash(token.right))
        }
        .getOrElse(formatOps.tokens.last)
    FormatTokenRange(start, end)
  }
}
