package org.scalafmt

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

class Formatter(style: ScalaStyle,
                owners: Map[Token, Tree]) extends ScalaFmtLogger {

  lazy val GetSplits = Default orElse Fail

  val Fail: PartialFunction[FormatToken, List[Split]] = {
    case tok =>
      logger.debug(
        s"""
           |=========== FAIL ============
           |${log(tok.left)}
           |${log(tok.between: _*)}
           |${log(tok.right)}""".stripMargin)
      ???
  }

  val Default: PartialFunction[FormatToken, List[Split]] = {
    case FormatToken(_: BOF, _, _) => List(
      NoSplitFree
    )
    case FormatToken(_, _: EOF, _) => List(
      NoSplitFree
    )
    case FormatToken(_, _: `,`, _) => List(
      NoSplitFree
    )
    case FormatToken(_: `,`, _, _) => List(
      SpaceFree,
      Newline0
    )
    case FormatToken(_: `{`, _: `}`, _) => List(
      NoSplitFree
    )
    case FormatToken(open: `{`, _, _) => List(
      oneLinerBlock(open),
      multiLineBlock(open)
    )
    case FormatToken(_, _: `{`, _) => List(
      SpaceFree
    )
    case FormatToken(_, _: `;`, _) => List(
      NoSplitFree
    )
    case FormatToken(_: `;`, _, _) => List(
      Newline(0, 0)
    )
    case FormatToken(_, _: `:`, _) => List(
      NoSplitFree
    )
    case FormatToken(_, _: `=`, _) => List(
      SpaceFree,
      Newline(3, 2)
    )
    case FormatToken(_: `:` | _: `=`, _, _) => List(
      SpaceFree
    )
    case FormatToken(_, _: `@`, _) => List(
      Newline0
    )
    case FormatToken(_: `@`, _, _) => List(
      NoSplitFree
    )
    case FormatToken(_: Ident, _: `.` | _: `#`, _) => List(
      NoSplitFree
    )
    case FormatToken(_: `.` | _: `#`, _: Ident, _) => List(
      NoSplitFree
    )
    case FormatToken(_: Ident | _: Literal, _: Ident | _: Literal, _) => List(
      SpaceFree
    )
    case FormatToken(_, _: `)` | _: `]`, _) => List(
      NoSplitFree
    )
    case FormatToken(_, _: `(` | _: `[`, _) => List(
      NoSplitFree
    )
    case FormatToken(_: `(` | _: `[`, _, _) => List(
      NoSplitFree,
      Newline0
    )
    case FormatToken(_, _: `val` | _: `case`, _) => List(
      Newline0
    )
    case FormatToken(_: Keyword | _: Modifier, _, _) =>
      List(
        SpaceFree,
        Newline(100, 4)
      )
    case FormatToken(_, _: Keyword, _) =>
      List(
        SpaceFree,
        Newline(6, 3)
      )
    case FormatToken(_, c: Comment, _) => List(
      SpaceFree
    )
    case FormatToken(c: Comment, _, _) =>
      if (c.code.startsWith("//")) List(Newline0)
      else List(SpaceFree, Newline0)
    case FormatToken(_, _: Delim, _) => List(
      SpaceFree
    )
    case FormatToken(_: Delim, _, _) => List(
      SpaceFree
    )
    // TODO(olafur) Ugly hack. Is there a better way?
    case tok if tok.left.name.startsWith("xml") &&
      tok.right.name.startsWith("xml") => List(
      NoSplitFree
    )
  }

  def oneLinerBlock(open: `{`): Split =
    Space(1, {
      // Disallow newlines inside block
      case Decision(tok, splits) =>
        Decision(tok, splits.filterNot(_.isInstanceOf[Newline]))
    })

  def multiLineBlock(open: `{`): Split =
    Newline(2, 2, {
      case Decision(tok@FormatToken(_, close: `}`, _), _)
        if owners.get(open) == owners.get(close) =>
        Decision(tok, List(Newline_2))
      case decision => decision
    })
}

