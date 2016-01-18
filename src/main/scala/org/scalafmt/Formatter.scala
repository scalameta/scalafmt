package org.scalafmt

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

class Formatter(style: ScalaStyle,
                owners: Map[Token, Tree]) extends ScalaFmtLogger {

  lazy val GetSplits = Default orElse Fail

  val Fail: PartialFunction[FormatToken, List[Split]] = {
    case tok =>
      logger.debug(log(tok))
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
      SpaceFree(),
      Newline0
    )
    case FormatToken(_: `{`, _: `}`, _) => List(
      NoSplitFree
    )
    case FormatToken(_: `}`, _: Keyword, _) => List(
      new Newline(0, 0)
    )
    case FormatToken(open: `{`, right, _) =>
      List(
        SingeLineBlock(1, open, owners),
        MultiLineBlock(2, open, owners)
      )
    case FormatToken(_, _: `{`, _) => List(
      SpaceFree()
    )
    case FormatToken(_, _: `;`, _) => List(
      NoSplitFree
    )
    case FormatToken(_: `;`, _, _) => List(
      new Newline(0, 0)
    )
    case FormatToken(_, _: `:`, _) => List(
      NoSplitFree
    )
    case FormatToken(_, tok: `=`, _) =>
      List(
        new Space(2),
        BreakStatement(3, tok, owners)
      )
    case tok@ FormatToken(_: `:` | _: `=`, _, _) => List(
      SpaceFree(),
      BreakStatement(5, tok.left, owners)
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
      SpaceFree()
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
    case tok@FormatToken(_: `val`, _, _) =>
      List(
        SpaceFree(),
        BreakStatement(6, tok.left, owners)
      )
    case tok@FormatToken(_: Keyword | _: Modifier, _, _) =>
      List(
        SpaceFree(),
        BreakStatement(4, tok.left, owners)
      )
    case FormatToken(_, _: Keyword, _) =>
      List(
        SpaceFree(),
        new Newline(2, 0)
      )
    case FormatToken(_, c: Comment, _) => List(
      SpaceFree()
    )
    case FormatToken(c: Comment, _, _) =>
      if (c.code.startsWith("//")) List(Newline0)
      else List(SpaceFree(), Newline0)
    case FormatToken(_, _: Delim, _) => List(
      SpaceFree()
    )
    case FormatToken(_: Delim, _, _) => List(
      SpaceFree()
    )
    // TODO(olafur) Ugly hack. Is there a better way?
    case tok if tok.left.name.startsWith("xml") &&
      tok.right.name.startsWith("xml") => List(
      NoSplitFree
    )
  }
}

