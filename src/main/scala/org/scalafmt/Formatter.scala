package org.scalafmt

import scala.annotation.tailrec
import scala.language.implicitConversions
import scala.meta.Tree
import scala.meta.internal.ast.Case
import scala.meta.internal.ast.Decl
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pat
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Term.ApplyUnary
import scala.meta.internal.ast.Term.Interpolate
import scala.meta.internal.ast.Type
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

class Formatter(style: ScalaStyle,
                tree: Tree,
                toks: Array[FormatToken],
                matching: Map[Token, Token],
                statementStarts: Map[Token, Tree],
                owners: Map[Token, Tree]) extends ScalaFmtLogger {

  import Split._

  val tok2idx = toks.zipWithIndex.toMap
  /**
    * Assigns possible splits to a FormatToken.
    *
    * The FormatToken can be considered as a node in a graph and the
    * splits as edges. Given a format token (a node in the graph), Route
    * determines which edges lead out from the format token.
    */
  val Route: PartialFunction[FormatToken, List[Split]] = {
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
      val close = matching(open)
      val p: Policy = {
          case Decision(t@FormatToken(_, `close`, _), s) =>
            Decision(t, List(Newline0))
        }
      val multiline = Split(nl, 1, p).withIndent(2, close, Right)
      // Optimization. Don't try single line if gap is too big.
      if (close.start - open.end < 50)
        List(
          Split(Space, 0, SingleLineBlock(close)),
          multiline
        )
      else List(multiline)
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
      if c.code.startsWith("/*") => List(
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
    // Opening [ with no leading space.
    case FormatToken(left, open: `[`, _)
      if owners(open).isInstanceOf[Term.ApplyType] ||
        owners(left).isInstanceOf[Type.Name] => List(
      NoSplit0
    )
    // Opening ( with no leading space.
    case FormatToken(left, open: `(`, _)
      if owners(open).isInstanceOf[Term.Apply] ||
        owners(left).parent.exists(_.isInstanceOf[Defn.Def]) ||
        owners(left).parent.exists(_.isInstanceOf[Defn.Class]) =>
      List(
        NoSplit0
      )
    // NOTE. && and || are infix applications, this case is before ApplyInfix.
    case FormatToken(cond: Ident, _, _)
      if cond.code == "&&" || cond.code == "||" =>
      List(
        Space0,
        Split(Newline, 1)
      )
    // DefDef
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
      // TODO(olafur) put some contraint on where newlines can appear.
        Split(Space, 0, SingleLineBlock(expire)),
        Split(Newline, 0).withIndent(2, expire, Left)
      )
    case tok@FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Defn] ||
        owners(open).parent.exists(_.isInstanceOf[Defn.Class]) =>
      List(
        Split(NoSplit, 0).withIndent(4, matching(open), Left)
      )
    case FormatToken(open: `[`, _, _)
      if !owners(open).isInstanceOf[Defn.Def] =>
      val expire = matching(open)
      val singleLine = SingleLineBlock(expire)
      val oneArgOneLine = OneArgOneLineSplit(open)
      List(
        Split(NoSplit, 0, policy = singleLine),
        Split(Newline, 1, singleLine).withIndent(4, expire, Left),
        Split(NoSplit, 2, oneArgOneLine).withIndent(StateColumn, expire, Left),
        Split(Newline, 3, oneArgOneLine).withIndent(4, expire, Left)
      )
    // TODO(olafur) Violating DRY with previous case.
    // I prefer to handle them separately.
    case FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Term.Apply] =>
      val close = matching(open)
      val singleLine = SingleLineBlock(close)
      val oneArgOneLine = OneArgOneLineSplit(open)
      val expire = matching(open)
      List(
        Split(NoSplit, 0, policy = singleLine),
        Split(Newline, 1, singleLine).withIndent(4, expire, Left),
        Split(NoSplit, 2, oneArgOneLine).withIndent(StateColumn, expire, Left),
        Split(Newline, 3, oneArgOneLine).withIndent(4, expire, Left)
      )

    // Delim
    case FormatToken(_, _: `,`, _) => List(
      NoSplit0
    )
    // These are mostly filtered out/modified by policies.
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
    // Only allow space after = in val if rhs is a single line or not
    // an infix application or an if. For example, this is allowed:
    // val x = function(a,
    //                  b)
    case FormatToken(tok: `=`, _, _)
      if owners(tok).isInstanceOf[Defn.Val] =>
      val owner = owners(tok).asInstanceOf[Defn.Val]
      val expire = owners(tok).tokens.last
      val isInfix = owner.rhs.isInstanceOf[Term.ApplyInfix]
      val isIfStmt = owner.rhs.isInstanceOf[Term.If]
      val spacePolicy: Policy = owner.rhs match {
        case _: Term.ApplyInfix | _: Term.If =>
          SingleLineBlock(expire)
        case _ => NoPolicy
      }
      List(
        Split(Space, 0, spacePolicy),
        Split(Newline, 1).withIndent(2, expire, Left)
      )
    case FormatToken(_: Ident | _: `this` | _: `_ ` | _: `)`, _: `.` | _: `#`, _) => List(
      NoSplit0
    )
    case FormatToken(_: `.` | _: `#`, _: Ident, _) => List(
      NoSplit0
    )
    // ApplyUnary
    case tok@FormatToken(_: Ident, _: Literal, _)
      if owners(tok.left) == owners(tok.right) =>
      List(
        NoSplit0
      )
    case tok@FormatToken(_: Ident, _: Ident | _: `this`, _)
      if owners(tok.left).parent.exists(_.isInstanceOf[ApplyUnary]) =>
      List(
        NoSplit0
      )
    // Annotations
    // TODO(olafur) what about "case t@Foobar =>..."?
    case FormatToken(_: `@`, _, _) => List(
      NoSplit0
    )
    // Template
    case FormatToken(_, right: `extends`, _) =>
      val owner = owners(right)
      val expire = owner.tokens
        .find(_.isInstanceOf[`{`])
        .getOrElse(owner.tokens.last)
      List(
        Split(Space, 0).withIndent(4, expire, Right)
      )
    case FormatToken(_, _: `with`, _) => List(
      Space0,
      Split(Newline, 1)
    )
    // If
    case FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Term.If] =>
      val owner = owners(open).asInstanceOf[Term.If]
      val close = matching(open)
      val forceNewlineBeforeThen: Policy = {
        case d@Decision(FormatToken(`close`, c: Comment, between), s)
          // TODO(olafur) handle these inline comments in a better way. DRY please.
          if c.code.startsWith("//") && !between.exists(_.isInstanceOf[`\n`]) =>
          val splits = s.map(_.withIndent(2, owner.thenp.tokens.last, Left))
          d.copy(split = splits)
        case Decision(tok@FormatToken(`close`, _, _), s) =>
        Decision(tok, List(
          Split(Newline, 0).withIndent(2, owner.thenp.tokens.last, Left)
        ))
      }
      List(
        Split(NoSplit, 0,  SingleLineBlock(owner.thenp.tokens.last))
          .withIndent(StateColumn, close, Left),
        Split(NoSplit, 1, forceNewlineBeforeThen)
          .withIndent(StateColumn, close, Left)
    )
    case tok@FormatToken(_: `=`, _, _)
      if nextNonComment(tok).right.isInstanceOf[`if`] =>
      val expire = owners(nextNonComment(tok).right).tokens.last
      List(
      Split(Space, 0, SingleLineBlock(expire)),
      Split(Newline, 1).withIndent(2, expire, Left)
    )
    // ApplyInfix.
    case FormatToken(_: Ident | _: Literal | _: Interpolation.End,
    _: Ident | _: Literal, _) => List(
      Space0
    )
    case FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Term.ApplyInfix] =>
      List(
        Split(NoSplit, 0, SingleLineBlock(matching(open))),
        Split(Newline, 1).withIndent(2, matching(open), Left)
      )

    // Pattern matching
    case tok@FormatToken(_, open: `(`, _)
      if owners(open).isInstanceOf[Pat.Extract] => List(
      NoSplit0
    )
    case tok@FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Pat] =>
      val close = matching(open)
      val singleLine = SingleLineBlock(close)
      val oneArgOneLine = OneArgOneLineSplit(open)
      val expire = matching(open)
      List(
        Split(NoSplit, 0, policy = singleLine),
        Split(Newline, 1, singleLine).withIndent(2, expire, Left),
        Split(NoSplit, 2, oneArgOneLine).withIndent(StateColumn, expire, Left),
        Split(Newline, 3, oneArgOneLine).withIndent(2, expire, Left)
      )
    // Case
    case tok@FormatToken(_, _: `match`, _) => List(
      Space0
    )
    case tok@FormatToken(cs: `case`, _, _)
      if owners(cs).isInstanceOf[Case] =>
      val owner = owners(cs).asInstanceOf[Case]
      val arrow = owner.tokens.find(t => t.isInstanceOf[`=>`] && owners(t) == owner).get
      // TODO(olafur) expire on token.end to avoid this bug.
      val lastToken = owner.body.tokens
        .filterNot(_.isInstanceOf[Whitespace])
        // edge case, if body is empty expire on arrow.
        .lastOption.getOrElse(arrow)
      val breakOnArrow: Policy = {
        case Decision(tok@FormatToken(`arrow`, _, _), s) =>
          Decision(tok, s.filter(_.modification.isNewline))
      }
      List(
        Split(Space, 0, {
          case Decision(t, s)
            if tok.right.end <= lastToken.end =>
            Decision(t, s.map{
              case nl if nl.modification.isNewline =>
                val result =
                  if (t.right.isInstanceOf[`if`] && owners(t.right) == owner) nl
                  else nl.withPenalty(1)
                result.withPolicy(breakOnArrow)
              case x => x
            })
        }).withIndent(2, lastToken, Left)
      )
    case tok@FormatToken(_, cond: `if`, _)
      if owners(cond).isInstanceOf[Case] =>
      val owner = owners(cond).asInstanceOf[Case]
      val arrow = owner.tokens.find(t => t.isInstanceOf[`=>`] && owners(t) == owner).get
      // TODO(olafur) bug in scala.meta cond tokens.
      val p: Policy = {
        case Decision(t, s) if t.left.code != "||" =>
          Decision(t, s.map {
            case nl if nl.modification.isNewline =>
              nl.withPenalty(1)
            case x => x
          })
      }
      List(
        Split(Space, 0, p),
        Split(Newline, 0, p).withIndent(2, arrow, Left)
      )
    case tok@FormatToken(arrow: `=>`, _, _)
      if owners(arrow).isInstanceOf[Case] =>
      val owner = owners(arrow).asInstanceOf[Case]
      val expire = owner.body.tokens
        .filterNot(_.isInstanceOf[Whitespace])
        .lastOption.getOrElse(arrow)
      List(
        Split(Space, 0, SingleLineBlock(expire)),
        Split(Newline, 1)
      )
    // Fallback
    case FormatToken(_: Keyword | _: Modifier, _, _) => List(
      Space0
    )
    case FormatToken(_, _: Keyword, _) => List(
      Newline0
    )
    case FormatToken(_, _: Delim, _) => List(
      Space0
    )
    case FormatToken(_: Delim, _, _) => List(
      Space0
    )
    case tok =>
      logger.debug("MISSING CASE:\n" + log(tok))
      List() // No solution available, partially format tree.
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
    if (!statementStarts.contains(tok.right)) false
    else {
      val owner = statementStarts(tok.right)
      if (!owner.tokens.headOption.contains(tok.right)) false
      else owner match {
        case _: Defn.Def | _: Pkg.Object |
             _: Defn.Class | _: Defn.Object |
             _: Defn.Trait => true
        case _ => false
      }
    }
  }

  def next(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == toks.length - 1) tok
    else toks(i + 1)
  }

  def OneArgOneLineSplit(open: Delim): Policy = {
    // Newline on every comma.
    case Decision(t@FormatToken(comma: `,`, right, between), splits)
      if owners.get(open) == owners.get(comma) &&
        // If comment is bound to comma, see unit/Comment.
        (!right.isInstanceOf[Comment] ||
          between.exists(_.isInstanceOf[`\n`])) =>
      Decision(t, splits.filter(_.modification == Newline))
  }

  def MultiLineBlock(close: Token): Policy = {
    case Decision(tok@FormatToken(_, `close`, _), splits) =>
      Decision(tok, splits.filter(_.modification == Newline))
  }

  def SingleLineBlock(expire: Token): Policy = {
    case Decision(tok, splits)
      if tok.right.end <= expire.end =>
      Decision(tok, splits.filterNot(_.modification.isNewline))
  }

  // Used for convenience when calling withIndent.
  private implicit def int2num(n: Int): Num = Num(n)
}

