package org.scalafmt.util

import org.scalafmt.config.FormatEvent.CreateFormatOps

import scala.meta.Defn
import scala.meta.Pkg
import scala.meta.Template
import scala.meta.Tree
import scala.meta.dialects.Scala211
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Modification
import org.scalafmt.internal.Newline
import org.scalafmt.internal.Newline2x
import org.scalafmt.internal.NewlineT
import org.scalafmt.internal.NoSplit
import org.scalafmt.internal.Policy
import org.scalafmt.internal.Space
import org.scalafmt.internal.Split

/**
  * Stateless helper functions on [[scala.meta.Token]].
  */
object TokenOps {

  /**
    * For convenience when experimenting with different hashing strategies.
    */
  type TokenHash = Long

  /**
    * Custom hash code for token.
    *
    * The default hashCode is slow because it prevents conflicts between
    * tokens from different source files. We only care about getting a unique
    * identifier for the token inside this source file.
    *
    * The hash code works like this this:
    * Top 8 bits go to a hashCode of productPrefix, a unique identifier for the tokens class.
    * Next 28 bits go to the tokens **start** offset byte.
    * Final 28 bits go to the tokens **end** offset byte.
    *
    * The only chance for collision is if two empty length tokens with the same
    * type lie next to each other. @xeno-by said this should not happen.
    */
  @inline
  def hash(token: Token): TokenHash = {
    val longHash: Long =
      (token.productPrefix.hashCode.toLong << (62 - 8)) |
        (token.start.toLong << (62 - (8 + 28))) | token.end
    longHash
  }

  def shouldGet2xNewlines(tok: FormatToken, style: ScalafmtConfig, owners: Token => Tree): Boolean = {
    !isDocstring(tok.left) && {
      val newlines = newlinesBetween(tok.between)
      newlines > 1 || (isDocstring(tok.right) && !tok.left.is[Comment])
    } || {
      style.newlines.alwaysBeforeTopLevelStatements &&
        tok.between.count(_.is[KwNew]) < 2 && isTopLevelStatment(tok.right, owners(tok.right))
    }
  }

  def isTopLevelStatment(tok: Token, owner: Tree): Boolean = {
    tok.is[KwObject] || owner.parent.exists(_.is[Defn.Object]) ||
      tok.is[KwClass] || owner.parent.exists(_.is[Defn.Class]) ||
      tok.is[KwDef] || owner.parent.exists(_.is[Defn.Def]) ||
      tok.is[KwTrait] || owner.parent.exists(_.is[Defn.Trait]) ||
      tok.is[KwPackage] || (tok.is[KwProtected] && owner.parent.exists(_.is[Pkg]))
  }

  def isDocstring(token: Token): Boolean = {
    token.is[Comment] && token.syntax.startsWith("/**")
  }

  def lastToken(tree: Tree): Token = {
    val lastIndex = tree.tokens.lastIndexWhere {
      case Trivia() | _: EOF => false
      case _ => true
    }
    if (lastIndex == -1) tree.tokens.last
    else tree.tokens(Scala211)(lastIndex)
  }

  def endsWithNoIndent(between: Vector[Token]): Boolean =
    between.lastOption.exists(_.is[LF])

  def rhsIsCommentedOut(formatToken: FormatToken): Boolean =
    formatToken.right.is[Comment] &&
      formatToken.right.syntax.startsWith("//") &&
      endsWithNoIndent(formatToken.between)

  val booleanOperators = Set("&&", "||")

  // TODO(olafur) more general solution?
  val newlineOkOperators = Set("+", "-", "|")

  def isBoolOperator(token: Token): Boolean =
    booleanOperators.contains(token.syntax)

  def newlineOkOperator(token: Token): Boolean =
    booleanOperators.contains(token.syntax) ||
      newlineOkOperators.contains(token.syntax)

  // See http://scala-lang.org/files/archive/spec/2.11/06-expressions.html#assignment-operators
  val specialAssignmentOperators = Set("<=", ">=", "!=")

  def isAssignmentOperator(token: Token): Boolean = {
    val code = token.syntax
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
    val lastCharacter = ident.syntax.last
    if (Character.isLetterOrDigit(lastCharacter) || lastCharacter == '`')
      NoSplit
    else Space
  }

  def isOpenApply(token: Token, includeCurly: Boolean = false): Boolean =
    token match {
      case LeftParen() | LeftBracket() => true
      case LeftBrace() if includeCurly => true
      case _ => false
    }

  /**
    * Forces allssplits up to including expire to be on a single line.
    */
  def SingleLineBlock(expire: Token,
                      exclude: Set[Range] = Set.empty,
                      disallowSingleLineComments: Boolean = true,
                      penaliseNewlinesInsideTokens: Boolean = false)(
      implicit line: sourcecode.Line): Policy = {
    Policy(
      {
        case Decision(tok, splits)
            if !tok.right.is[EOF] && tok.right.end <= expire.end &&
              exclude.forall(!_.contains(tok.left.start)) &&
              (disallowSingleLineComments || !isSingleLineComment(tok.left)) =>
          if (penaliseNewlinesInsideTokens && tok.leftHasNewline) {
            Decision(tok, Seq.empty[Split])
          } else {
            Decision(tok, splits.filterNot(_.modification.isNewline))
          }
      },
      expire.end,
      noDequeue = exclude.isEmpty,
      isSingleLine = true
    )
  }

  def isSingleLineComment(token: Token): Boolean = token match {
    case c: Comment => c.syntax.startsWith("//")
    case _ => false
  }

  def newlines2Modification(formatToken: FormatToken): Modification =
    newlines2Modification(formatToken.between, formatToken.right.is[Comment])

  def newlines2Modification(between: Vector[Token],
                            rightIsComment: Boolean = false): Modification =
    newlinesBetween(between) match {
      case 0 => Space
      case x =>
        NewlineT(isDouble = x == 2,
                 noIndent = rightIsComment && endsWithNoIndent(between))
    }

  // TODO(olafur) calculate this once inside getSplits.

  def newlinesBetween(between: Vector[Token]): Int =
    between.count(_.is[LF])

  def isAttachedSingleLineComment(token: Token, between: Vector[Token]) =
    isSingleLineComment(token) && newlinesBetween(between) == 0

  def defnTemplate(tree: Tree): Option[Template] = tree match {
    case t: Defn.Object => Some(t.templ)
    case t: Defn.Class => Some(t.templ)
    case t: Defn.Trait => Some(t.templ)
    case t: Pkg.Object => Some(t.templ)
    case t: Template => Some(t)
    case _ => None
  }

  def tokenLength(token: Token): Int = token match {
    case lit: Constant.String =>
      // Even if the literal is not strip margined, we use the longest line
      // excluding margins. The will only affect is multiline string literals
      // with a short first line but long lines inside, example:
      //
      // val x = """short
      //  Long aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
      // """
      //
      // In this case, we would put a newline before """short and indent by
      // two.
      lit.syntax.lines.map(_.replaceFirst(" *|", "").length).max
    case _ =>
      val tokenSyntax = token.syntax
      val firstNewline = tokenSyntax.indexOf('\n')
      if (firstNewline == -1) tokenSyntax.length
      else firstNewline
  }

  def isFormatOn(token: Token): Boolean = token match {
    case c: Comment if formatOnCode.contains(c.syntax.toLowerCase) => true
    case _ => false
  }

  def isFormatOff(token: Token): Boolean = token match {
    case c: Comment if formatOffCode.contains(c.syntax.toLowerCase) => true
    case _ => false
  }

  val formatOffCode = Set(
    "// @formatter:off", // IntelliJ
    "// format: off" // scalariform
  )

  def endsWithSymbolIdent(tok: Token): Boolean = tok match {
    case Ident(name) => !name.last.isLetter
    case _ => false
  }

  def isSymbolicIdent(tok: Token): Boolean = tok match {
    case Ident(name) =>
      val head = name.head
      // DESNOTE(2017-02-03, pjrt) Variable names can start with a letter or
      // an `_`, operators cannot. This check should suffice.
      !head.isLetter && head != '_'
    case _ => false
  }

  val formatOnCode = Set(
    "// @formatter:on", // IntelliJ
    "// format: on" // scalariform
  )
}
