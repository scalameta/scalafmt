package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

class Formatter(style: ScalaStyle,
                owners: Map[Token, Tree]) extends ScalaFmtLogger {

  import Split._

  lazy val GetSplits = Default orElse Fail

  val Default: PartialFunction[FormatToken, List[Split]] = {
    case FormatToken(_: BOF, _, _) => List(
      NoSplit0
    )
    case FormatToken(_, _: EOF, _) => List(
      NoSplit0
    )
    case FormatToken(_, _: `,`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `,`, _, _) => List(
      Space0,
      Newline0
    )
    case FormatToken(_: `{`, _: `}`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `}`, _: Keyword, _) => List(
      Newline0
    )
    case FormatToken(open: `{`, right, _) =>
      List(
        Split(Space, 1, policy = SingleLineBlock(open)),
        Split(Newline, 2, indent = Push(2),
          policy = MultiLineBlock(open))
      )
    case FormatToken(_, _: `{`, _) => List(
      Space0
    )
    case FormatToken(_, _: `}`, _) => List(
      Space0,
      Newline0
    )
    case FormatToken(_, _: `;`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `;`, _, _) => List(
      Newline0
    )
    case FormatToken(_, _: `:`, _) => List(
      NoSplit0
    )
    case FormatToken(_, tok: `=`, _) =>
      List(
        Split(Space, 2),
        BreakStatement(3, tok)
      )
    case tok@FormatToken(_: `:` | _: `=`, _, _) => List(
      Space0,
      BreakStatement(5, tok.left)
    )
    case FormatToken(_: Ident, _: `.` | _: `#`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `.` | _: `#`, _: Ident, _) => List(
      NoSplit0
    )
    case FormatToken(_: Ident | _: Literal, _: Ident | _: Literal, _) => List(
      Space0
    )
    case FormatToken(_, _: `)` | _: `]`, _) => List(
      NoSplit0
    )
    case FormatToken(_, _: `(` | _: `[`, _) => List(
      NoSplit0
    )
    case tok@FormatToken(_: `(` | _: `[`, _, _) =>
      val open = tok.left.asInstanceOf[Delim]
      val singleLine = SingleLineBlock(open)
      val oneArgOneLine = OneArgOneLineSplit(open)
      List(
        Split(NoSplit, 0, policy = singleLine),
        Split(Newline, 1, Push(4),
          Unindent(open) orElse singleLine),
        Split(NoSplit, 2, PushStateColumn, oneArgOneLine),
        Split(Newline, 3, Push(4), oneArgOneLine)
      )
    case FormatToken(_, _: `@`, _) => List(
      Newline0
    )
    case FormatToken(_: `@`, _, _) => List(
      NoSplit0
    )
    case FormatToken(_, _: `val` | _: `case`, _) => List(
      Newline0
    )
    case tok@FormatToken(_: `val`, _, _) =>
      List(
        Space0,
        BreakStatement(6, tok.left)
      )
    case tok@FormatToken(_: Keyword | _: Modifier, _, _) =>
      List(
        Space0,
        BreakStatement(4, tok.left)
      )
    case FormatToken(_, _: Keyword, _) =>
      List(
        Space0,
        Split(Newline, 2)
      )
    case FormatToken(_, c: Comment, _) => List(
      Space0
    )
    case FormatToken(c: Comment, _, _) =>
      if (c.code.startsWith("//")) List(Newline0)
      else List(Space0, Newline0)
    case FormatToken(_, _: Delim, _) => List(
      Space0
    )
    case FormatToken(_: Delim, _, _) => List(
      Space0
    )
    case tok if tok.left.name.startsWith("xml") &&
      tok.right.name.startsWith("xml") => List(
      NoSplit0
    )
    case tok if tok.left.getClass.getName.contains("Interpolation") ||
      tok.right.getClass.getName.contains("Interpolation") => List(
      NoSplit0
    )
  }

  val Fail: PartialFunction[FormatToken, List[Split]] = {
    case tok =>
      logger.debug(log(tok))
      ???
  }

  def OneArgOneLineSplit(open: Delim): Policy =
    Unindent(open) orElse {
      // Newline on every comma.
      case Decision(t@FormatToken(comma: `,`, _, _), splits)
        if owners.get(open) == owners.get(comma) =>
        Decision(t, splits.filter(_.modification == Newline))
    }

  def Unindent(open: Delim): Policy = {
    case Decision(t@FormatToken(close: `)`, _, _), splits)
      if open.isInstanceOf[`(`] && owners.get(open) == owners.get(close) =>
      Decision(t, splits.map(_.withIndent(Pop)))
    case Decision(t@FormatToken(close: `]`, _, _), splits)
      if open.isInstanceOf[`[`] && owners.get(open) == owners.get(close) =>
      Decision(t, splits.map(_.withIndent(Pop)))
  }

  def MultiLineBlock(open: `{`): Policy = {
    case Decision(tok@FormatToken(_, close: `}`, _), splits)
      if owners.get(open) == owners.get(close) =>
      Decision(tok, splits.withFilter(_.modification == Newline).map {
        case s => s.withIndent(Pop)
      })
  }


  def SingleLineBlock(open: Delim): Policy = {
    val end = owners(open).tokens.last.end
    val policy: Policy = {
      case Decision(tok, splits)
        if tok.right.end <= end =>
        Decision(tok, splits.filterNot(_.modification == Newline))
    }
    policy
  }

  def BreakStatement(cost: Int, tok: Token): Split = {
    val parent = owners(tok)
    Split(Newline, cost, Push(2), {
      case Decision(t@FormatToken(left, right, _), s)
        if childOf(left, parent, owners) && !childOf(right, parent, owners) =>
        Decision(t, s.map(_.withIndent(Pop)))
    })
  }
}

