package org.scalafmt.internal

import scala.meta.Import
import scala.meta.Pat
import scala.meta.dialects.Scala211
import scala.meta.tokens.Tokens

import org.scalafmt.internal.ExpiresOn.Left
import org.scalafmt.internal.Policy.NoPolicy
import org.scalafmt.Error.CaseMissingArrow
import org.scalafmt.ScalafmtStyle
import org.scalafmt.ScalafmtRunner
import org.scalafmt.internal.Length.Num
import org.scalafmt.util.LoggerOps
import org.scalafmt.util.TokenOps
import org.scalafmt.util.TreeOps
import org.scalafmt.util.Delim
import org.scalafmt.util.Whitespace
import scala.annotation.tailrec
import scala.collection.mutable
import scala.meta.Tree
import scala.meta.Case
import scala.meta.Defn
import scala.meta.Pkg
import scala.meta.Template
import scala.meta.Term
import scala.meta.Type
import scala.meta.prettyprinters.Structure
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

/**
  * Helper functions for generating splits/policies for a given tree.
  */
class FormatOps(val tree: Tree,
                val style: ScalafmtStyle,
                val runner: ScalafmtRunner) {
  import LoggerOps._
  import TokenOps._
  import TreeOps._

  val tokens: Array[FormatToken] = FormatToken.formatTokens(tree.tokens)
  val ownersMap = getOwners(tree)
  val statementStarts = getStatementStarts(tree)
  val dequeueSpots = getDequeueSpots(tree) ++ statementStarts.keys
  val matchingParentheses = getMatchingParentheses(tree.tokens)

  @inline
  def owners(token: Token): Tree = ownersMap(hash(token))
  /*
   * The tokens on the left hand side of Pkg
   *
   * For example Set(org, ., scalafmt) in:
   *
   * package org.scalafmt
   *
   * import foo.bar
   * ...
   *
   */
  val (packageTokens, importTokens, argumentStarts) = {
    val packages = Set.newBuilder[Token]
    val imports = Set.newBuilder[Token]
    val b = mutable.Map.empty[TokenHash, Tree]
    def add(tree: Tree): Unit = {
      if (tree.tokens.nonEmpty && !b.contains(hash(tree.tokens.head))) {
        b += hash(tree.tokens.head) -> tree
      }
    }
    def iter(tree: Tree): Unit = {
      tree match {
        case p: Pkg => packages ++= p.ref.tokens
        case i: Import => imports ++= i.tokens
        case t: Term.Arg => add(t)
        case t: Term.Param => add(t)
        case _ =>
      }
      tree.children.foreach(iter)
    }
    iter(tree)
    (packages.result(), imports.result(), b.toMap)
  }

  lazy val leftTok2tok: Map[Token, FormatToken] = {
    val result = Map.newBuilder[Token, FormatToken]
    result.sizeHint(tokens.length)
    tokens.foreach(t => result += t.left -> t)
    result += (tokens.last.right -> tokens.last)
    result.result()
  }

  lazy val tok2idx: Map[FormatToken, Int] = tokens.zipWithIndex.toMap

  def prev(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == 0) tok
    else tokens(i - 1)
  }

  def next(tok: FormatToken): FormatToken = {
    val i = tok2idx(tok)
    if (i == tokens.length - 1) tok
    else tokens(i + 1)
  }

  @tailrec
  final def findFirst(start: FormatToken, end: Token)(
      f: FormatToken => Boolean): Option[FormatToken] = {
    if (start.left.start < end.start) None
    else if (f(start)) Some(start)
    else {
      val next_ = next(start)
      if (next_ == start) None
      else findFirst(next_, end)(f)
    }
  }

  @tailrec
  final def nextNonComment(curr: FormatToken): FormatToken = {
    if (!curr.right.is[Comment]) curr
    else {
      val tok = next(curr)
      if (tok == curr) curr
      else nextNonComment(tok)
    }
  }

  @tailrec
  final def rhsOptimalToken(start: FormatToken): Token = start.right match {
    case Comma() | LeftParen() | RightParen() | RightBracket() | Semicolon() |
        RightArrow()
        if next(start) != start &&
          !owners(start.right).tokens.headOption.contains(start.right) &&
          newlinesBetween(start.between) == 0 =>
      rhsOptimalToken(next(start))
    case _ => start.left
  }

  /**
    * js.native is very special in Scala.js.
    *
    * Context: https://github.com/olafurpg/scalafmt/issues/108
    */
  def isJsNative(jsToken: Token): Boolean = {
    style.noNewlinesBeforeJsNative && jsToken.syntax == "js" &&
    owners(jsToken).parent.exists(
      _.show[Structure].trim == """Term.Select(Term.Name("js"), Term.Name("native"))""")
  }

  def isTripleQuote(token: Token): Boolean = token.syntax.startsWith("\"\"\"")

  def isMarginizedString(token: Token): Boolean = token match {
    case start @ Interpolation.Start() =>
      val end = matchingParentheses(hash(start))
      val afterEnd = next(leftTok2tok(end))
      afterEnd.left.syntax == "." && afterEnd.right.syntax == "stripMargin"
    case string: Constant.String =>
      string.syntax.startsWith("\"\"\"") && {
        val afterString = next(leftTok2tok(string))
        afterString.left.syntax == "." &&
        afterString.right.syntax == "stripMargin"
      }
    case _ => false
  }

  @tailrec
  final def startsStatement(tok: FormatToken): Boolean = {
    statementStarts.contains(hash(tok.right)) ||
    (tok.right.is[Comment] && tok.between.exists(_.is[LF]) &&
    startsStatement(next(tok)))
  }

  def parensRange(open: Token): Range =
    Range(open.start, matchingParentheses(hash(open)).end)

  def getExcludeIf(end: Token,
                   cond: Token => Boolean = _.is[RightBrace]): Set[Range] = {
    if (cond(end)) // allow newlines in final {} block
      Set(Range(matchingParentheses(hash(end)).start, end.end))
    else Set.empty[Range]
  }

  def skipUnindent(token: Token): Boolean = {
    token.is[LeftParen] && {
      val owner = owners(token)
      val isSuperfluous = isSuperfluousParenthesis(token, owner)
      isSuperfluous && (owner match {
        case _: Term.ApplyUnary | _: Term.Block => false
        case _ => true
      })

    }
  }

  def insideBlock(start: FormatToken,
                  end: Token,
                  matches: Token => Boolean): Set[Token] = {
    val result = Set.newBuilder[Token]
    var prev = start
    var curr = next(start)

    def goToMatching(): Unit = {
      val close = matchingParentheses(hash(curr.left))
      curr = leftTok2tok(close)
    }

    while (curr.left.start < end.start && curr != prev) {
      if (matches(curr.left)) {
        result += curr.left
        goToMatching()
      } else {
        prev = curr
        curr = next(curr)
      }
    }
    result.result()
  }

  def defnSiteLastToken(tree: Tree): Token = {
    tree match {
      // TODO(olafur) scala.meta should make this easier.
      case procedure: Defn.Def
          if procedure.decltpe.isDefined &&
            procedure.decltpe.get.tokens.isEmpty =>
        procedure.body.tokens.find(_.is[LeftBrace])
      case _ => tree.tokens.find(t => t.is[Equals] && owners(t) == tree)
    }
  }.getOrElse(tree.tokens.last)

  def OneArgOneLineSplit(open: Token, noTrailingCommas: Boolean = false)(
      implicit line: sourcecode.Line): Policy = {
    // TODO(olafur) clear queue between arguments, they are independent.
    val expire = matchingParentheses(hash(open))
    Policy({
      case d @ Decision(t @ FormatToken(left, comma @ Comma(), _), splits)
          if noTrailingCommas &&
            !next(t).right.is[Comment] &&
            owners(open) == owners(comma) =>
        Decision(t, splits.map {
          case x if x.modification == NoSplit => x.copy(modification = Newline)
          case x => x
        })

      // Newline on every comma.
      case d @ Decision(t @ FormatToken(comma @ Comma(), right, between),
                        splits)
          if owners(open) == owners(comma) &&
            // TODO(olafur) what the right { decides to be single line?
            !right.is[LeftBrace] &&
            // If comment is bound to comma, see unit/Comment.
            (!right.is[Comment] ||
              between.exists(_.is[LF])) =>
        Decision(t, splits.filter { x =>
          val isNewline = x.modification.isNewline
          if (noTrailingCommas && !right.is[Comment]) !isNewline
          else isNewline
        })
    }, expire.end)
  }

  def UnindentAtExclude(
      exclude: Set[Token],
      indent: Length): PartialFunction[Decision, Decision] = {
    case Decision(t, s) if exclude.contains(t.left) =>
      val close = matchingParentheses(hash(t.left))
      Decision(t, s.map(_.withIndent(indent, close, ExpiresOn.Left)))
  }

  def penalizeAllNewlines(expire: Token,
                          penalty: Int,
                          penalizeLambdas: Boolean = true,
                          ignore: FormatToken => Boolean = _ => false,
                          penaliseNewlinesInsideTokens: Boolean = false)(
      implicit line: sourcecode.Line): Policy = {
    Policy({
      case Decision(tok, s)
          if tok.right.end < expire.end &&
            (penalizeLambdas || !tok.left.is[RightArrow]) && !ignore(tok) =>
        Decision(tok, s.map {
          case split
              if split.modification.isNewline ||
                (penaliseNewlinesInsideTokens && tok.leftHasNewline) =>
            split.withPenalty(penalty)
          case x => x
        })
    }, expire.end)
  }

  def penalizeNewlineByNesting(from: Token, to: Token)(
      implicit line: sourcecode.Line): Policy = {
    val range = Range(from.start, to.end).inclusive
    Policy({
      case Decision(t, s) if range.contains(t.right.start) =>
        val nonBoolPenalty =
          if (isBoolOperator(t.left)) 0
          else 5

        val penalty =
          nestedSelect(owners(t.left)) + nestedApplies(owners(t.right)) +
            nonBoolPenalty
        Decision(t, s.map {
          case split if split.modification.isNewline =>
            split.withPenalty(penalty)
          case x => x
        })
    }, to.end)
  }

  def getArrow(caseStat: Case): Token =
    caseStat.tokens
      .find(t => t.is[RightArrow] && owners(t) == caseStat)
      .getOrElse(throw CaseMissingArrow(caseStat))

  def templateCurly(owner: Tree): Token = {
    defnTemplate(owner).flatMap(templateCurly).getOrElse(owner.tokens.last)
  }

  def templateCurly(template: Template): Option[Token] = {
    template.tokens.find(x => x.is[LeftBrace] && owners(x) == template)
  }

  def safeFilterNewlines(splits: Seq[Split])(
      implicit line: sourcecode.Line): Seq[Split] = {
    val filtered = splits.filter(_.modification.isNewline)
    if (filtered.nonEmpty) filtered
    else Seq(Split(Newline, 0))
  }

  final def getElseChain(term: Term.If): Vector[KwElse] = {
    term.tokens.find(x => x.is[KwElse] && owners(x) == term) match {
      case Some(els @ KwElse()) =>
        val rest = term.elsep match {
          case t: Term.If => getElseChain(t)
          case _ => Vector.empty[KwElse]
        }
        els +: rest
      case _ => Vector.empty[KwElse]
    }
  }

  def lastTokenInChain(chain: Vector[Term.Select]): Token = {
    if (chain.length == 1) lastToken(chain.last)
    else chainOptimalToken(chain)
  }

  /**
    * Returns last token of select, handles case when select's parent is apply.
    *
    * For example, in:
    * foo.bar[T](1, 2)
    * the last token is the final )
    *
    * @param dot the dot owned by the select.
    */
  def getSelectsLastToken(dot: Dot): Token = {
    var curr = next(leftTok2tok(dot))
    while (isOpenApply(curr.right, includeCurly = true) &&
           !statementStarts.contains(hash(curr.right))) {
      curr = leftTok2tok(matchingParentheses(hash(curr.right)))
    }
    curr.left
  }

  def getRightAttachedComment(token: Token): Token = {
    val formatToken = leftTok2tok(token)
    if (isAttachedComment(formatToken.right, formatToken.between))
      formatToken.right
    else token
  }

  def chainOptimalToken(chain: Vector[Term.Select]): Token = {
    val lastDotIndex = chain.last.tokens.lastIndexWhere(_.is[Dot])
    val lastDot =
      if (lastDotIndex != -1)
        chain.last.tokens(Scala211)(lastDotIndex).asInstanceOf[Dot]
      else
        throw new IllegalStateException(s"Missing . in select ${chain.last}")
    lastToken(owners(getSelectsLastToken(lastDot)))
  }

  def infixSplit(
      owner: Tree,
      op: Term.Name,
      rhsArgs: Seq[Tree],
      formatToken: FormatToken)(implicit line: sourcecode.Line): Split = {
    val modification = newlines2Modification(
      formatToken.between,
      rightIsComment = formatToken.right.isInstanceOf[Comment])
    val indent = {
      if ((style.unindentTopLevelOperators ||
          isTopLevelInfixApplication(owner)) &&
          (style.indentOperator.includeRegexp
            .findFirstIn(op.tokens.head.syntax)
            .isEmpty ||
          style.indentOperator.excludeRegexp
            .findFirstIn(op.tokens.head.syntax)
            .isDefined)) 0
      else if (!modification.isNewline &&
               !isAttachedComment(formatToken.right, formatToken.between)) 0
      else 2
    }
    val expire = (for {
      arg <- rhsArgs.lastOption
      token <- arg.tokens.lastOption
    } yield token).getOrElse(owner.tokens.last)
    Split(modification, 0).withIndent(Num(indent), expire, ExpiresOn.Left)
  }

  /**
    * Returns the expire token for the owner of dot.
    *
    * If the select is part of an apply like
    *
    * foo.bar { ... }
    *
    * the expire token is the closing }, otherwise it's bar.
    */
  def selectExpire(dot: Dot): Token = {
    val owner = ownersMap(hash(dot))
    (for {
      parent <- owner.parent
      (_, args) <- splitApplyIntoLhsAndArgsLifted(parent) if args.nonEmpty
    } yield {
      args.last.tokens.last
    }).getOrElse(owner.tokens.last)
  }

  def functionExpire(function: Term.Function): Token = {
    (for {
      parent <- function.parent
      blockEnd <- parent match {
        case b: Term.Block if b.stats.length == 1 =>
          Some(b.tokens.last)
        case _ => None
      }
    } yield blockEnd).getOrElse(function.tokens.last)
  }

  def noOptimizationZones(tree: Tree): Set[Token] = {
    val result = Set.newBuilder[Token]
    var inside = false
    var expire = tree.tokens.head
    tree.tokens.foreach {
      case t if !inside && ((t, ownersMap(hash(t))) match {
            case (LeftParen(), _: Term.Apply) =>
              // TODO(olafur) https://github.com/scalameta/scalameta/issues/345
              val x = true
              x
            // Type compounds can be inside defn.defs
            case (LeftBrace(), _: Type.Compound) => true
            case _ => false
          }) =>
        inside = true
        expire = matchingParentheses(hash(t))
      case x if x == expire => inside = false
      case x if inside => result += x
      case _ =>
    }
    result.result()
  }

  def opensConfigStyle(formatToken: FormatToken): Boolean = {
    newlinesBetween(formatToken.between) > 0 && {
      val close = matchingParentheses(hash(formatToken.left))
      newlinesBetween(prev(leftTok2tok(close)).between) > 0
    }
  }
  def getApplyIndent(leftOwner: Tree, isConfigStyle: Boolean = false): Num =
    leftOwner match {
      case _: Pat => Num(0) // Indentation already provided by case.
      case x if isDefnSite(x) && !x.isInstanceOf[Type.Apply] =>
        if (style.binPackParameters && !isConfigStyle) Num(0)
        else Num(style.continuationIndentDefnSite)
      case _ => Num(style.continuationIndentCallSite)
    }

  def isBinPack(owner: Tree): Boolean = {
    (style.binPackArguments && isCallSite(owner)) ||
    (style.binPackParameters && isDefnSite(owner))
  }

  def isSingleIdentifierAnnotation(tok: FormatToken): Boolean = {
    val toMatch = if (tok.right.is[RightParen]) {
      // Hack to allow any annotations with arguments like @foo(1)
      prev(prev(leftTok2tok(matchingParentheses(hash(tok.right)))))
    } else {
      tok
    }
    toMatch match {
      case FormatToken(At(), _: Ident, _) => true
      case _ => false
    }
  }

  // Maps token to number of non-whitespace bytes before the token's position.
  private final val nonWhitespaceOffset: Map[Token, Int] = {
    val resultB = Map.newBuilder[Token, Int]
    var curr = 0
    tree.tokens.foreach {
      case t =>
        resultB += (t -> curr)
        if (!t.is[Whitespace]) {
          curr += (t.end - t.start)
        }

    }
    resultB.result()
  }

  def distance(left: Token, right: Token): Int = {
    nonWhitespaceOffset(right) - nonWhitespaceOffset(left)
  }

  def breakOnEveryWith(owner: Option[Tree], lastToken: Token) = {
    if (style.binPackParentConstructors) NoPolicy
    else {
      Policy({
        case Decision(t @ FormatToken(_, right @ KwWith(), _), splits)
            if owner == ownersMap.get(hash(right)) =>
          Decision(t, splits.filter(_.modification.isNewline))
      }, lastToken.end)
    }
  }

  def binPackParentConstructorSplits(
      owner: Option[Tree],
      lastToken: Token,
      indent: Int)(implicit line: sourcecode.Line) = {
    Seq(
      Split(Space, 0)
        .withPolicy(SingleLineBlock(lastToken))
        .withIndent(Num(indent), lastToken, Left),
      Split(NewlineT(acceptSpace = true), 1)
        .withPolicy(breakOnEveryWith(owner, lastToken))
        .withIndent(Num(indent), lastToken, Left)
    )
  }

  def newlineBeforeClosingCurlyPolicy(close: Token) =
    Policy({
      case d @ Decision(t @ FormatToken(_, `close`, _), s) =>
        d.onlyNewlines
    }, close.end)
}
