package org.scalafmt.internal

import scala.meta.Tree
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.internal.ast.Defn
import scala.meta.internal.ast.Pkg
import scala.meta.internal.ast.Template

object TokenOps extends TokenOps

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
    formatToken.right.code.startsWith("//") &&
    endsWithNoIndent(formatToken.between)

  val booleanOperators = Set("&&", "||")

  // TODO(olafur) more general solution?
  val newlineOkOperators = Set("+", "-")

  def isBoolOperator(token: Token): Boolean =
    booleanOperators.contains(token.code)

  def newlineOkOperator(token: Token): Boolean =
    booleanOperators.contains(token.code) ||
    newlineOkOperators.contains(token.code)

  // See http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#assignment-operators
  val specialAssignmentOperators = Set("<=", ">=", "!=")

  def isAssignmentOperator(token: Token): Boolean = {
    val code = token.code
    code.last == '=' && code.head != '=' &&
    !specialAssignmentOperators.contains(code)
  }

  val symbolOperatorPrecendence: PartialFunction[Char, Int] = {
    case '|' => 2
    case '^' => 3
    case '&' => 4
    case '=' | '!' => 5
    case '<' | '>' => 6
    case ':' => 7
    case '+' | '-' => 8
    case '*' | '/' | '%' => 9
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

  /**
    * Forces allssplits up to including expire to be on a single line.
    */
  def SingleLineBlock(expire: Token,
                      exclude: Set[Range] = Set.empty,
                      disallowInlineComments: Boolean = true)(
      implicit line: sourcecode.Line): Policy = {
    Policy({
      case Decision(tok, splits)
          if !tok.right.isInstanceOf[EOF] && tok.right.end <= expire.end &&
          exclude.forall(!_.contains(tok.left.start)) &&
          (disallowInlineComments || !isInlineComment(tok.left)) =>
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

  def isFormatOn(token: Token): Boolean =
    token match {
      case c: Comment if formatOnCode.contains(c.code.toLowerCase) => true
      case _ => false
    }

  def isFormatOff(token: Token): Boolean =
    token match {
      case c: Comment if formatOffCode.contains(c.code.toLowerCase) => true
      case _ => false
    }

  val formatOffCode = Set(
      "// @formatter:off", // IntelliJ
      "// format: off" // scalariform
  )

  val formatOnCode = Set(
      "// @formatter:on", // IntelliJ
      "// format: on" // scalariform
  )
}
