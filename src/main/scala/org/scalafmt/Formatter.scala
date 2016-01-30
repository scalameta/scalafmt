package org.scalafmt

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.meta.Tree
import scala.meta.internal.ast.Decl
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Term.Interpolate
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

class Formatter(style: ScalaStyle,
                tree: Tree,
                toks: Array[FormatToken],
                matching: Map[Token, Token],
                statementStarts: Set[Token],
                owners: Map[Token, Tree]) extends ScalaFmtLogger {

  import Split._


  // Used for convenience when calling withIndent.
  private implicit def int2num(n: Int): Num = Num(n)

  lazy val GetSplits = Default orElse Fail

  val tok2idx = toks.zipWithIndex.toMap
  val Default: PartialFunction[FormatToken, List[Split]] = {
    case FormatToken(_: BOF, _, _) => List(
      NoSplit0
    )
    case FormatToken(_, _: EOF, _) => List(
      NoSplit0
    )
    case tok if tok.left.name.startsWith("xml") &&
      tok.right.name.startsWith("xml") => List(
      NoSplit0
    )
    case tok if owners(tok.left).isInstanceOf[Interpolate] &&
      owners(tok.right).isInstanceOf[Interpolate] =>
      List(
        NoSplit0
      )
    case FormatToken(_: `{`, _: `}`, _) => List(
      NoSplit0
    )
    case tok@FormatToken(open: `{`, _, _) =>
      val nl: Modification = if (gets2x(tok)) Newline2x else Newline
      List(
        Split(Space, 0, policy = SingleLineBlock(open)),
        Split(nl, 1, MultiLineBlock(open))
          .withIndent(2, matching(open), Right)
      )
    case FormatToken(_, _: `{`, _) => List(
      Space0
    )
    case tok: FormatToken if !isDocstring(tok.left) && gets2x(tok) => List(
      Split(Newline2x, 0)
    )
    case FormatToken(_, right, _) if statementStarts.contains(right) => List(
      Newline0
    )
    case FormatToken(_, _: `}`, _) => List(
      Space0,
      Newline0
    )
    case FormatToken(_, _: `import`, _) =>
      List(Newline0)
    case FormatToken(c: Comment, _, _)
      if c.code.startsWith("//") =>
      List(Newline0)
    case FormatToken(_, c: Comment, between)
      if c.code.startsWith("//") =>
      val newlineCounts = between.count(_.isInstanceOf[`\n`])
      if (newlineCounts > 1) List(Split(Newline2x, 0))
      else if (newlineCounts == 1) List(Newline0)
      else List(Space0)
    case FormatToken(_, c: Comment, _)
      if c.code.startsWith("/**") => List(
      Split(Newline2x, 0)
    )
    case FormatToken(left: `package `, _, _)
      if owners(left).isInstanceOf[Pkg] =>
      val owner = owners(left).asInstanceOf[Pkg]
      val lastRef = owner.ref.tokens.last
      List(
        Split(Space, 0, policy = {
          // Following case:
          // package foo // this is cool
          //
          // object a
          case Decision(t@FormatToken(`lastRef`, _: Comment, between), splits)
            if !between.exists(_.isInstanceOf[`\n`]) =>
            Decision(t, splits.map(_.withModification(Space)))
          case Decision(t@FormatToken(`lastRef`, _, _), splits) =>
            Decision(t, splits.map(_.withModification(Newline2x)))
        })
      )
    case FormatToken(_, _: `)` | _: `]`, _) => List(
      NoSplit0
    )
    case FormatToken(_, _: `(` | _: `[`, _) => List(
      NoSplit0
    )
//     TODO(olafur) Naive match, can be 1) comment between 2) abstract decl.
    case tok@FormatToken(d: `def`, name: Ident, _) =>
      val owner = owners(d)
      val expire = owner.tokens.
        find(t => t.isInstanceOf[`=`] && owners(t) == owner).get
      List(
        Split(Space, 0).withIndent(4, expire, Left)
      )
    case FormatToken(e: `=`, _, _)
      if owners(e).isInstanceOf[Defn.Def] =>
      val expire = owners(e).tokens.last
      List(
        Space0,
        Split(Newline, 0).withIndent(2, expire, Left)
      )
    case tok@FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Defn.Def] =>
      val owner = owners(open)
      List(
        Split(NoSplit, 0)
      )
    case tok@FormatToken(_: `(` | _: `[`, _, _)
      if !owners(tok.left).isInstanceOf[Defn.Def] =>
      val open = tok.left.asInstanceOf[Delim]
      val singleLine = SingleLineBlock(open)
      val oneArgOneLine = OneArgOneLineSplit(open)
      val expire = matching(open)
      List(
        Split(NoSplit, 0, policy = singleLine),
        Split(Newline, 1, singleLine).withIndent(4, expire, Left),
        Split(NoSplit, 2, oneArgOneLine).withIndent(StateColumn, expire, Left),
        Split(Newline, 3, oneArgOneLine).withIndent(4, expire, Left)
      )
    case FormatToken(_, _: `,`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `,`, _, _) => List(
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
    case FormatToken(_, tok: `=`, _)
      if !owners(tok).isInstanceOf[Defn.Def] =>
      val expire = owners(tok).tokens.last
      List(
        Split(Space, 2),
        Split(Newline, 3).withIndent(2, expire, Left)
      )
    case tok@FormatToken(left: `:`, _, _)
      if owners(left).isInstanceOf[Defn.Val] =>
      val expire = owners(left).tokens.last
      List(
      Space0,
        Split(Newline, 5).withIndent(2, expire, Left)
    )
    case FormatToken(_: Ident | _: `this`, _: `.` | _: `#`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `.` | _: `#`, _: Ident, _) => List(
      NoSplit0
    )
    case FormatToken(_: Ident | _: Literal | _: Interpolation.End,
    _: Ident | _: Literal, _) => List(
      Space0
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
      val expire = owners(tok.left).tokens.last
      List(
        Space0,
        Split(Newline, 6).withIndent(Num(2), expire, Left)
      )
    case tok@FormatToken(_: Keyword | _: Modifier, _, _) => List(
      Space0
    )
    case FormatToken(left, right: Keyword, _) =>
      if (owners(left) == owners(right)) List(Space0)
      else List(Newline0)
    case FormatToken(_, _: Delim, _) => List(
      Space0
    )
    case FormatToken(_: Delim, _, _) => List(
      Space0
    )
  }
  val Fail: PartialFunction[FormatToken, List[Split]] = {
    case tok =>
      logger.debug("MISSING CASE:\n" + log(tok))
      ???
  }

  def isDocstring(token: Token): Boolean = {
    token.isInstanceOf[Comment] && token.code.startsWith("/**")
  }

  @tailrec
  final def nextNonComment(curr: FormatToken): FormatToken = {
    if (!curr.right.isInstanceOf[Comment]) curr
    else {
      val tok = next(curr)
      if (tok == curr) curr
      else nextNonComment(tok)
    }
  }

  def gets2x(formatToken: FormatToken): Boolean = {
    val tok = nextNonComment(formatToken)
    val owner = owners(tok.right)
    if (!owner.tokens.headOption.contains(tok.right)) false
    else owner match {
      case _: Defn.Def | _: Pkg.Object |
           _: Defn.Class | _: Defn.Object |
           _: Defn.Trait => true
      case _ => false
    }
  }

  def next(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == toks.length - 1) tok
    else toks(i + 1)
  }

  def prev(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == 0) tok
    else toks(i - 1)
  }

  def OneArgOneLineSplit(open: Delim): Policy = {
      // Newline on every comma.
      case Decision(t@FormatToken(comma: `,`, _, _), splits)
        if owners.get(open) == owners.get(comma) =>
        Decision(t, splits.filter(_.modification == Newline))
    }

  def MultiLineBlock(open: `{`): Policy = {
    case Decision(tok@FormatToken(_, close: `}`, _), splits)
      if owners.get(open) == owners.get(close) =>
      Decision(tok, splits.filter(_.modification == Newline))
  }


  def SingleLineBlock(open: Delim): Policy = {
    val end = owners(open).tokens.last.end
    val policy: Policy = {
      case Decision(tok, splits)
        if tok.right.end <= end =>
        Decision(tok, splits.filterNot(_.modification.isNewline))
    }
    policy
  }

  def getLastTokenInDef(d: Tree): Token = d match {
    case defn: Defn.Def =>
      defn.body.tokens.head
    case defn: Decl.Def =>
      defn.decltpe.tokens.last
    case _ =>
      ???
  }
}

