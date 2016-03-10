package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Template

/**
  * Stateless helper functions on [[scala.meta.Token]].
  */
trait TokenOps extends ScalaFmtLogger {

  def shouldGet2xNewlines(tok: FormatToken): Boolean = {
    !isDocstring(tok.left) && {
      val newlines = newlinesBetween(tok.between)
      newlines > 1 || (isDocstring(tok.right) &&
          !tok.left.isInstanceOf[Comment])
    }
  }

  def isDocstring(token: Token): Boolean = {
    token.isInstanceOf[Comment] && token.code.startsWith("/**")
  }

  def lastToken(tree: Tree): Token = {
    val lastIndex = tree.tokens.lastIndexWhere {
      case _: Trivia | _: EOF => false
      case _ => true
    }
    if (lastIndex == - 1) tree.tokens.last
    else tree.tokens(lastIndex)
  }

  def endsWithNoIndent(between: Vector[Whitespace]): Boolean =
    between.lastOption.exists(_.isInstanceOf[`\n`])

  def rhsIsCommentedOut(formatToken: FormatToken): Boolean =
    formatToken.right.isInstanceOf[Comment] &&
    endsWithNoIndent(formatToken.between)

  def isBoolOperator(token: Token): Boolean =
    token.code match {
      case "||" | "&&" => true
      case _ => false
    }

  def identModification(ident: Ident): Modification = {
    val lastCharacter = ident.code.last
    if (Character.isLetterOrDigit(lastCharacter) || lastCharacter == '`')
      NoSplit
    else Space
  }

  def isOpenApply(token: Token): Boolean =
    token match {
      case _: `(` | _: `[` => true
      case _ => false
    }

  def isSingleIdentifierAnnotation(tok: FormatToken): Boolean =
    tok match {
      case FormatToken(_: `@`, _: Ident, _) => true
      case _ => false
    }

  def SingleLineBlock(expire: Token,
                      exclude: Set[Range] = Set.empty,
                      killInlineComments: Boolean = true)(
      implicit line: sourcecode.Line): Policy = {
    Policy({
      case Decision(tok, splits)
          if !tok.right.isInstanceOf[EOF] && tok.right.end <= expire.end &&
          exclude.forall(!_.contains(tok.left.start)) &&
          (killInlineComments || !isInlineComment(tok.left)) =>
        Decision(tok, splits.filterNot(_.modification.isNewline))
    }, expire.end, noDequeue = exclude.isEmpty)
  }

  def isInlineComment(token: Token): Boolean =
    token match {
      case c: Comment => c.code.startsWith("//")
      case _ => false
    }

  def newlines2Modification(between: Vector[Whitespace]): Modification =
    newlinesBetween(between) match {
      case 0 => Space
      case x => Newline(x == 2, endsWithNoIndent(between))
    }

  // TODO(olafur) calculate this once inside getSplits.

  def newlinesBetween(between: Vector[Whitespace]): Int =
    between.count(_.isInstanceOf[`\n`])

  def isAttachedComment(token: Token, between: Vector[Whitespace]) =
    isInlineComment(token) && newlinesBetween(between) == 0

  def defnTemplate(tree: Tree): Option[Template] =
    tree match {
      case t: Defn.Object => Some(t.templ)
      case t: Defn.Class => Some(t.templ)
      case t: Defn.Trait => Some(t.templ)
      case t: Pkg.Object => Some(t.templ)
      case _ => None
    }
}
