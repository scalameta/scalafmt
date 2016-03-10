package org.scalafmt.internal

import org.scalafmt.Error.CaseMissingArrow
import org.scalafmt.ScalaStyle

import scala.annotation.tailrec
import scala.meta.Tree
import scala.meta.internal.ast.Case
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.prettyprinters.Structure

/**
  * Helper functions for generating splits/policies for a given tree.
  */
trait FormatOps extends TreeOps {
  val style: ScalaStyle
  val tree: Tree
  val tokens: Array[FormatToken]
  val matchingParentheses: Map[TokenHash, Token]
  val statementStarts: Map[TokenHash, Tree]
  val ownersMap: Map[TokenHash, Tree]

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
  val packageTokens: Set[Token] = {
    val result = new scala.collection.mutable.SetBuilder[Token, Set[Token]](Set
      .empty[Token])
    tree.collect {
      case p: Pkg => result ++= p.ref.tokens
    }
    result.result()
  }

  val leftTok2tok: Map[Token, FormatToken] = tokens.map(t => t.left -> t).toMap
  val tok2idx: Map[FormatToken, Int] = tokens.zipWithIndex.toMap

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
    if (!curr.right.isInstanceOf[Comment]) curr
    else {
      val tok = next(curr)
      if (tok == curr) curr
      else nextNonComment(tok)
    }
  }

  def gets2x(tok: FormatToken): Boolean = {
    if (!statementStarts.contains(hash(tok.right))) false
    else if (packageTokens.contains(tok.left) &&
             !packageTokens.contains(tok.right)) true
    else {
      val rightOwner = statementStarts(hash(tok.right))
      if (!rightOwner.tokens.headOption.contains(tok.right)) false
      else if (!rightOwner.parent.exists(isTopLevel)) false
      else
        rightOwner match {
          case _: Defn.Def | _: Pkg.Object | _: Defn.Class | _: Defn.Object |
              _: Defn.Trait =>
            true
          case _ => false
        }
    }
  }

  def OneArgOneLineSplit(open: Delim)(implicit line: sourcecode.Line): Policy = {
    val expire = matchingParentheses(hash(open))
    Policy({
      // Newline on every comma.
      case Decision(t@FormatToken(comma: `,`, right, between), splits)
          if owners(open) == owners(comma) &&
          // TODO(olafur) what the right { decides to be single line?
          !right.isInstanceOf[`{`] &&
          // If comment is bound to comma, see unit/Comment.
          (!right.isInstanceOf[Comment] ||
              between.exists(_.isInstanceOf[`\n`])) =>
        Decision(t, splits.filter(_.modification.isNewline))
    }, expire.end)
  }
  @tailrec
  final def rhsOptimalToken(start: FormatToken): Token =
    start.right match {
      case _: `,` | _: `(` | _: `)` | _: `]` | _: `;` | _: `=>`
          if next(start) != start &&
          !owners(start.right).tokens.headOption.contains(start.right) =>
        rhsOptimalToken(next(start))
      case _ => start.left
    }

  /**
    * js.native is very special in Scala.js.
    *
    * Context: https://github.com/olafurpg/scalafmt/issues/108
    */
  def isJsNative(jsToken: Token): Boolean = {
    style == ScalaStyle.ScalaJs && jsToken.code == "js" &&
    owners(jsToken).parent.exists(
        _.show[Structure].trim == """Term.Select(Term.Name("js"), Term.Name("native"))""")
  }

  @tailrec
  final def startsStatement(tok: FormatToken): Boolean = {
    statementStarts.contains(hash(tok.right)) ||
    (tok.right.isInstanceOf[Comment] &&
        tok.between.exists(_.isInstanceOf[`\n`]) && startsStatement(next(tok)))
  }

  def insideBlock(start: FormatToken, end: Token): Set[Range] = {
    var inside = false
    val result = new scala.collection.mutable.SetBuilder[Range, Set[Range]](Set
      .empty[Range])
    var curr = start
    while (curr.left != end) {
      if (curr.left.isInstanceOf[`{`]) {
        inside = true
        result += Range(
            curr.left.start, matchingParentheses(hash(curr.left)).end)
        curr = leftTok2tok(matchingParentheses(hash(curr.left)))
      } else {
        curr = next(curr)
      }
    }
    result.result()
  }

  def defnSiteLastToken(tree: Tree): Token = {
    tree.tokens.find(t => t.isInstanceOf[`=`] && owners(t) == tree)
      .getOrElse(tree.tokens.last)
  }

  def penalizeNewlineByNesting(from: Token, to: Token)(
      implicit line: sourcecode.Line): Policy = {
    val range = Range(from.start, to.end).inclusive
    Policy({
      case Decision(t, s) if range.contains(t.right.start) =>
        // TODO(olafur) overfitting unit test?
        val nonBoolPenalty =
          if (isBoolOperator(t.left)) 0
          else 1
        val penalty =
          nestedSelect(owners(t.left)) + nestedApplies(owners(t.right)) + nonBoolPenalty
        Decision(t, s.map {
          case split if split.modification.isNewline =>
            split.withPenalty(penalty)
          case x => x
        })
    }, to.end)
  }

  def getArrow(caseStat: Case): Token =
    caseStat.tokens.find(t => t.isInstanceOf[`=>`] && owners(t) == caseStat)
      .getOrElse(throw CaseMissingArrow(caseStat))
}
