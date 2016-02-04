package org.scalafmt

import scala.annotation.tailrec
import scala.collection.mutable
import scala.language.implicitConversions
import scala.meta.Tree
import scala.meta.internal.ast.Case
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Import
import scala.meta.internal.ast.Pat
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Term
import scala.meta.internal.ast.Term.ApplyUnary
import scala.meta.internal.ast.Term.Interpolate
import scala.meta.internal.ast.Type
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

class Formatter(style: ScalaStyle,
                tree: Tree,
                toks: Array[FormatToken],
                matching: Map[Token, Token],
                statementStarts: Map[Token, Tree],
                owners: Map[Token, Tree]) extends ScalaFmtLogger {
  private val leftTok2tok: Map[Token, FormatToken] = toks.map(t => t.left -> t).toMap
  private val tok2idx: Map[FormatToken, Int] = toks.zipWithIndex.toMap
  private val cache = mutable.Map.empty[FormatToken, List[Split]]
  // TODO(olafur) move into function, store owners(left/right), etc.
  private val RouteRun: PartialFunction[FormatToken, List[Split]] = {
    case FormatToken(_: BOF, _, _) => List(
      Split(NoSplit, 0)
    )
    case FormatToken(_, _: EOF, _) => List(
      Split(NoSplit, 0)
    )
    case tok if tok.left.name.startsWith("xml") &&
      tok.right.name.startsWith("xml") => List(
      Split(NoSplit, 0)
    )
    case tok if owners(tok.left).isInstanceOf[Interpolate] &&
      owners(tok.right).isInstanceOf[Interpolate] =>
      List(
        Split(NoSplit, 0)
      )
    case FormatToken(_: `{`, _: `}`, _) => List(
      Split(NoSplit, 0)
    )
    case FormatToken(_: `]`, _: `(`, _) => List(
      Split(NoSplit, 0)
    )
    case tok@FormatToken(open: `{`, right, between) =>
      val nl: Modification = if (gets2x(tok)) Newline2x else Newline
      val close = matching(open)
      val blockSize = close.start - open.end
      val ignore = blockSize > style.maxColumn || isInlineComment(right)
      List(
        Split(Space, 0, ignoreIf = ignore)
          .withPolicy(SingleLineBlock(close)),
        Split(nl, 1).withPolicy {
          case Decision(t@FormatToken(_, `close`, _), s) =>
            Decision(t, List(Split(Newline, 0)))
        }.withIndent(2, close, Right)
      )
    case FormatToken(_: `(`, _: `{`, _) => List(
      Split(NoSplit, 0)
    )
    case FormatToken(_, _: `{`, _) => List(
      Split(Space, 0)
    )
    case tok: FormatToken if !isDocstring(tok.left) && gets2x(tok) => List(
      Split(Newline2x, 0)
    )
    case FormatToken(_, right, _) if statementStarts.contains(right) => List(
      Split(Newline, 0)
    )
    case FormatToken(_, _: `}`, _) => List(
      Split(Space, 0),
      Split(Newline, 0)
    )
    case FormatToken(_, _: `import`, _) =>
      List(Split(Newline, 0))
    case FormatToken(c: Comment, _, between)
      if c.code.startsWith("//") =>
      if (between.lastOption.exists(_.isInstanceOf[`\n`]))
        List(Split(NoIndentNewline, 0))
      else List(Split(Newline, 0))
    case FormatToken(_, c: Comment, between)
      if c.code.startsWith("//") =>
      val newlineCounts = between.count(_.isInstanceOf[`\n`])
      if (newlineCounts > 1) List(Split(Newline2x, 0))
      else if (newlineCounts == 1) List(Split(Newline, 0))
      else List(Split(Space, 0))
    case FormatToken(_, c: Comment, _)
      if c.code.startsWith("/*") => List(
      Split(Newline2x, 0)
    )
    case FormatToken(left: `package `, _, _)
      if owners(left).isInstanceOf[Pkg] =>
      val owner = owners(left).asInstanceOf[Pkg]
      val lastRef = owner.ref.tokens.last
      List(
        Split(Space, 0).withPolicy {
          // Following case:
          // package foo // this is cool
          //
          // object a
          case Decision(t@FormatToken(`lastRef`, _: Comment, between), splits)
            if !between.exists(_.isInstanceOf[`\n`]) =>
            Decision(t, splits.map(_.withModification(Space)))
          case Decision(t@FormatToken(`lastRef`, _, _), splits) =>
            Decision(t, splits.map(_.withModification(Newline2x)))
        }
      )
    case FormatToken(_, _: `)` | _: `]`, _) => List(
      Split(NoSplit, 0)
    )
    // Opening [ with no leading space.
    case FormatToken(left, open: `[`, _)
      if owners(open).isInstanceOf[Term.ApplyType] ||
        owners(left).isInstanceOf[Type.Name] => List(
      Split(NoSplit, 0)
    )
    // Opening ( with no leading space.
    case FormatToken(left, open: `(`, _)
      if owners(open).isInstanceOf[Term.Apply] ||
        owners(left).parent.exists(_.isInstanceOf[Defn.Def]) ||
        owners(left).parent.exists(_.isInstanceOf[Defn.Class]) =>
      List(
        Split(NoSplit, 0)
      )
    // NOTE. && and || are infix applications, this case is before ApplyInfix.
    case FormatToken(cond: Ident, _, _)
      if cond.code == "&&" || cond.code == "||" =>
      List(
        Split(Space, 0),
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
        Split(Space, 0, policy = SingleLineBlock(expire)),
        Split(Newline, 0).withIndent(2, expire, Left)
      )
    case tok@FormatToken(open: `(`, _, _)
      if owners(open).isInstanceOf[Defn] ||
        owners(open).parent.exists(_.isInstanceOf[Defn.Class]) =>
      List(
        Split(NoSplit, 0).withIndent(4, matching(open), Left)
      )
    // TODO(olafur) Split this up.
    case tok@FormatToken(_: `(` | _: `[`, _, _)
      if owners(tok.left).isInstanceOf[Term.Apply] ||
        owners(tok.left).isInstanceOf[Pat.Extract] ||
        owners(tok.left).isInstanceOf[Pat.Tuple] ||
        owners(tok.left).isInstanceOf[Term.Tuple] ||
        owners(tok.left).isInstanceOf[Term.ApplyType] ||
        owners(tok.left).isInstanceOf[Type.Apply] =>
      val open = tok.left.asInstanceOf[Delim]
      val (lhs, args): (Tree, Seq[Tree]) = owners(tok.left) match {
        case t: Term.Apply => t.fun -> t.args
        case t: Pat.Extract => t.ref -> t.args
        case t: Pat.Tuple => t -> t.elements
        case t: Term.ApplyType => t -> t.targs
        case t: Term.Tuple => t -> t.elements
        case t: Type.Apply => t.tpe -> t.args
      }
      val close = matching(open)
      val expire = matching(open)
      // TODO(olafur) recursively?
      val optimalTok: Token = leftTok2tok(close) match {
        case FormatToken(_, right: `,`, _) => right
        case FormatToken(_, right: `)`, _) => right
        case FormatToken(_, right: `]`, _) => right
        case FormatToken(_, right: `;`, _) => right
        case FormatToken(_, right: `=>`, _) =>
          right
        case _ => close
      }
      // In long sequence of select/apply, we penalize splitting on
      // parens furthest to the right.
      val lhsPenalty = lhs.tokens.length
      val nestedPenalty = NestedApplies(owners(open))

      val exclude = insideBlock(tok, close)
      val indent = owners(open) match {
        case _: Pat => Num(0) // Indentation already provided by case.
        // TODO(olafur) This is an odd rule, when is it wrong?
        case _ => Num(4)
      }
      val singleArgument = args.length == 1

      val singleLine = // Don't force single line policy if only one argument.
        if (singleArgument) NoPolicy
        else SingleLineBlock(close, exclude)
      val oneArgOneLine = OneArgOneLineSplit(open)
      List(
        Split(NoSplit, 0, policy = singleLine)
          .withIndent(indent, expire, Left)
          .withOptimal(optimalTok),
        Split(Newline, 1 + nestedPenalty + lhsPenalty, policy = singleLine)
          .withIndent(indent, expire, Left)
          .withOptimal(optimalTok),
        Split(NoSplit, 2 + lhsPenalty, policy = oneArgOneLine, ignoreIf = singleArgument)
          .withIndent(StateColumn, expire, Right)
          .withOptimal(optimalTok),
        Split(Newline,
          3 + nestedPenalty + lhsPenalty,
          policy = oneArgOneLine, ignoreIf = singleArgument)
          .withIndent(indent, expire, Left)
      )

    // Delim
    case FormatToken(_, _: `,`, _) => List(
      Split(NoSplit, 0)
    )
    // These are mostly filtered out/modified by policies.
    case FormatToken(_: `,`, _, _) => List(
      Split(Space, 0),
      Split(Newline, 0)
    )
    case FormatToken(_, _: `;`, _) => List(
      Split(NoSplit, 0)
    )
    case FormatToken(_: `;`, _, _) => List(
      Split(Newline, 0)
    )
    case FormatToken(_, _: `:`, _) => List(
      Split(NoSplit, 0)
    )
    // Only allow space after = in val if rhs is a single line or not
    // an infix application or an if. For example, this is allowed:
    // val x = function(a,
    //                  b)
    case FormatToken(tok: `=`, _, _)
      // TODO(olafur) scala.meta should have uniform api for these two
      if owners(tok).isInstanceOf[Defn.Val] ||
      owners(tok).isInstanceOf[Defn.Var] =>
      val rhs = owners(tok) match {
        case l: Defn.Val => l.rhs
        case r: Defn.Var => r.rhs
      }
      val expire = owners(tok).tokens.last
      val spacePolicy: Policy = rhs match {
        case _: Term.ApplyInfix | _: Term.If =>
          SingleLineBlock(expire)
        case _ => NoPolicy
      }
      List(
        Split(Space, 0, policy = spacePolicy),
        Split(Newline, 1).withIndent(2, expire, Left)
      )
    case FormatToken(left, dot: `.`, _)
      if owners(dot).isInstanceOf[Term.Select] &&
        !left.isInstanceOf[`_ `] &&
        // TODO(olafur) optimize
        !parents(owners(dot)).exists(_.isInstanceOf[Import]) =>
      val nestedPenalty = NestedSelect(owners(dot))
      val isLhsOfApply = owners(dot).parent.exists {
        case apply: Term.Apply =>
          apply.fun.tokens.contains(left)
        case _ => false
      }
      val noApplyPenalty =
        if (!isLhsOfApply) 1
        else 0
      // TODO(olafur) missing optimalAt
      List(
        Split(NoSplit, 0),
        Split(Newline, 1 + nestedPenalty + noApplyPenalty)
          .withIndent(2, dot, Left)
      )
    case FormatToken(_: Ident | _: `this` | _: `_ ` | _: `)`, _: `.` | _: `#`, _) =>
      List(
      Split(NoSplit, 0)
    )
    case FormatToken(_: `.` | _: `#`, _: Ident, _) => List(
      Split(NoSplit, 0)
    )
    // ApplyUnary
    case tok@FormatToken(_: Ident, _: Literal, _)
      if owners(tok.left) == owners(tok.right) =>
      List(
        Split(NoSplit, 0)
      )
    case tok@FormatToken(_: Ident, _: Ident | _: `this`, _)
      if owners(tok.left).parent.exists(_.isInstanceOf[ApplyUnary]) =>
      List(
        Split(NoSplit, 0)
      )
    // Annotations
    case FormatToken(_, bind: `@`, _)
      if owners(bind).isInstanceOf[Pat.Bind] => List(
      Split(NoSplit, 0)
    )
    case FormatToken(_: `@`, _, _) => List(
      Split(NoSplit, 0)
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
      Split(Space, 0),
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
        Split(NoSplit, 0, policy = SingleLineBlock(owner.thenp.tokens.last))
          .withIndent(StateColumn, close, Left),
        Split(NoSplit, 1, policy = forceNewlineBeforeThen)
          .withIndent(StateColumn, close, Left)
      )
    case tok@FormatToken(_: `=`, _, _)
      if nextNonComment(tok).right.isInstanceOf[`if`] =>
      val ifOwner = owners(nextNonComment(tok).right)
      val expire = ifOwner.tokens.last
      List(
        Split(Space, 0,
          policy = SingleLineBlock(expire)
        ),
        Split(Newline, 1).withIndent(2, expire, Left)
      )
    // ApplyInfix.
    case FormatToken(left: Ident, _, _)
      if owners(left).isInstanceOf[Term.ApplyInfix] => List(
      Split(Space, 0),
      Split(Newline, 1)
    )
    case tok@FormatToken(_: Ident | _: Literal | _: Interpolation.End,
    _: Ident | _: Literal, _) => List(
      Split(Space, 0)
    )
    case FormatToken(open: `(`, right, _)
      if owners(open).isInstanceOf[Term.ApplyInfix] =>
      val close = matching(open)
      val policy =
        if (right.isInstanceOf[`if`]) SingleLineBlock(close)
        else NoPolicy
      List(
        Split(NoSplit, 0).withPolicy(policy).withIndent(2, matching(open), Left),
        Split(Newline, 1).withIndent(2, matching(open), Left)
      )

    // Pattern matching
    case tok@FormatToken(_, open: `(`, _)
      if owners(open).isInstanceOf[Pat.Extract] => List(
      Split(NoSplit, 0)
    )
    // Pat
    case tok@FormatToken(or: Ident, _, _)
      if or.code == "|" && owners(or).isInstanceOf[Pat.Alternative] => List(
      Split(Space, 0),
      Split(Newline, 0)
    )
    // Case
    case tok@FormatToken(_, _: `match`, _) => List(
      Split(Space, 0)
    )
    case tok@FormatToken(cs: `case`, _, _)
      if owners(cs).isInstanceOf[Case] =>
      val owner = owners(cs).asInstanceOf[Case]
      val arrow = owner.tokens.find(t => t.isInstanceOf[`=>`] && owners(t) == owner).get
      // TODO(olafur) expire on token.end to avoid this bug.
      val lastToken = owner.body.tokens
        .filter {
          case _: Whitespace | _: Comment => false
          case _ => true
        }
        // edge case, if body is empty expire on arrow.
        .lastOption.getOrElse(arrow)
      val breakOnArrow: Policy = {
        case Decision(tok@FormatToken(`arrow`, _, _), s) =>
          Decision(tok, s.filter(_.modification.isNewline))
      }
      List(
        Split(Space, 0).withPolicy {
          case Decision(t, s)
            if tok.right.end <= lastToken.end =>
            Decision(t, s.map {
              case nl if nl.modification.isNewline =>
                val result =
                  if (t.right.isInstanceOf[`if`] && owners(t.right) == owner) nl
                  else nl.withPenalty(1)
                result.withPolicy(breakOnArrow)
              case x => x
            })
        }.withIndent(2, lastToken, Left)
          .withIndent(2, arrow, Left)
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
        Split(Space, 0, policy = p),
        Split(Newline, 0, policy = p)
      )
    case tok@FormatToken(arrow: `=>`, _, _)
      if owners(arrow).isInstanceOf[Case] =>
      val owner = owners(arrow).asInstanceOf[Case]
      val expire = owner.body.tokens
        .filterNot(_.isInstanceOf[Whitespace])
        .lastOption.getOrElse(arrow)
      val bodySize = expire.start - tok.left.end
      List(
        Split(Space, 0,
          policy = SingleLineBlock(expire)
        ),
        Split(Newline, 1)
      )
    // Fallback
    case FormatToken(_: Keyword | _: Modifier, _, _) => List(
      Split(Space, 0)
    )
    case FormatToken(_, _: Keyword, _) => List(
      Split(Newline, 0)
    )
    case FormatToken(_, _: Delim, _) => List(
      Split(Space, 0)
    )
    case FormatToken(_: `[`, _, _) => List(
      Split(NoSplit, 0)
    )
    case FormatToken(_: Delim, _, _) => List(
      Split(Space, 0)
    )
    case tok =>
      logger.debug("MISSING CASE:\n" + log(tok))
      List() // No solution available, partially format tree.
  }

  /**
    * Assigns possible splits to a FormatToken.
    *
    * The FormatToken can be considered as a node in a graph and the
    * splits as edges. Given a format token (a node in the graph), Route
    * determines which edges lead out from the format token.
    */

  def Route(formatToken: FormatToken): List[Split] =
    cache.getOrElseUpdate(formatToken, RouteRun(formatToken))

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


  /**
    * How many parents of tree are Term.Apply?
    */
  def NestedApplies(tree: Tree): Int = {
    // TODO(olafur) optimize?
    tree.parent.fold(0) {
      case parent: Term.Apply =>
        1 + NestedApplies(parent)
      case parent =>
        NestedApplies(parent)
    }
  }

  // TODO(olafur) abstract with [[NestedApplies]]
  def NestedSelect(tree: Tree): Int = {
    tree.parent.fold(0) {
      case parent: Term.Select =>
        1 + NestedSelect(parent)
      case parent =>
        NestedSelect(parent)
    }
  }

  def SingleLineBlock(expire: Token,
                      exclude: Set[Range] = Set.empty): Policy = {
    case Decision(tok, splits)
      if exclude.forall(!_.contains(tok.left.start)) && tok.right.end <= expire.end =>
      Decision(tok, splits.filterNot(_.modification.isNewline))
  }


  def insideBlock(start: FormatToken, end: Token): Set[Range] = {
    var inside = false
    val result = mutable.Set.empty[Range]
    var curr = start
    while (curr.left != end) {
      if (curr.left.isInstanceOf[`{`]) {
        inside = true
        result += Range(curr.left.start, matching(curr.left).end)
        curr = leftTok2tok(matching(curr.left))
      }
      else {
        curr = next(curr)
      }
    }
    result.toSet
  }

  def isInlineComment(token: Token): Boolean = token match {
    case c: Comment => c.code.startsWith("//")
    case _ => false
  }


  // Used for convenience when calling withIndent.
  private implicit def int2num(n: Int): Num = Num(n)
}

