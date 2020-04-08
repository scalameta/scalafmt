package org.scalafmt.util

import scala.meta.{Defn, Pkg, Template, Tree}
import scala.meta.dialects.Scala211
import scala.meta.tokens.Token
import scala.meta.tokens.Token._

import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.Decision
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Modification
import org.scalafmt.internal.NewlineT
import org.scalafmt.internal.Policy
import org.scalafmt.internal.Space

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

  def shouldGet2xNewlines(
      tok: FormatToken
  )(implicit style: ScalafmtConfig): Boolean =
    tok.hasBlankLine || blankLineBeforeDocstring(tok.left, tok.right)

  def blankLineBeforeDocstring(
      left: Token,
      right: Token
  )(implicit style: ScalafmtConfig): Boolean =
    right.is[Token.Comment] &&
      blankLineBeforeDocstring(left, right.asInstanceOf[Token.Comment])

  def blankLineBeforeDocstring(
      left: Token,
      right: Token.Comment
  )(implicit style: ScalafmtConfig): Boolean =
    style.optIn.forceNewlineBeforeDocstringSummary &&
      !left.is[Token.Comment] && right.syntax.startsWith("/**")

  def lastToken(tree: Tree): Token = {
    // 2.13 has findLast
    val tokens = tree.tokens
    val index = tokens.lastIndexWhere {
      case Trivia() | _: EOF => false
      case _ => true
    }
    if (index == -1) tokens.last else tokens(index)
  }

  def endsWithNoIndent(between: Seq[Token]): Boolean =
    between.lastOption.exists(_.is[LF])

  def rhsIsCommentedOut(formatToken: FormatToken): Boolean =
    isSingleLineComment(formatToken.right) &&
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
    Space(!Character.isLetterOrDigit(lastCharacter) && lastCharacter != '`')
  }

  def isOpenApply(
      token: Token,
      includeCurly: Boolean = false,
      includeNoParens: Boolean = false
  ): Boolean =
    token match {
      case LeftParen() | LeftBracket() => true
      case LeftBrace() if includeCurly => true
      case Dot() if includeNoParens => true
      case _ => false
    }

  /**
    * Forces allssplits up to including expire to be on a single line.
    */
  def SingleLineBlock(
      expire: Token,
      exclude: Set[Range] = Set.empty,
      disallowSingleLineComments: Boolean = true,
      penaliseNewlinesInsideTokens: Boolean = false
  )(implicit line: sourcecode.Line): Policy = {
    Policy(
      {
        case d @ Decision(tok, _)
            if !tok.right.is[EOF] && tok.right.end <= expire.end &&
              exclude.forall(!_.contains(tok.left.start)) &&
              (disallowSingleLineComments || !isSingleLineComment(tok.left)) =>
          if (penaliseNewlinesInsideTokens && tok.leftHasNewline) {
            Seq.empty
          } else {
            d.noNewlines
          }
      },
      expire.end,
      noDequeue = true
    )
  }

  def isSingleLineComment(c: Token.Comment): Boolean =
    c.syntax.startsWith("//")

  def isSingleLineComment(token: Token): Boolean = token match {
    case c: Comment => isSingleLineComment(c)
    case _ => false
  }

  private def getModByNL(nl: Int, noIndent: => Boolean): Modification =
    if (FormatToken.noBreak(nl)) Space
    else {
      val isDouble = FormatToken.hasBlankLine(nl)
      NewlineT(isDouble = isDouble, noIndent = noIndent)
    }

  def getMod(ft: FormatToken, noIndent: Boolean = false): Modification =
    getModByNL(ft.newlinesBetween, noIndent)

  def getModCheckIndent(ft: FormatToken): Modification =
    getModCheckIndent(ft, ft.newlinesBetween)

  def getModCheckIndent(ft: FormatToken, newlines: Int): Modification =
    getModByNL(newlines, ft.right.is[Comment] && endsWithNoIndent(ft.between))

  def isAttachedSingleLineComment(ft: FormatToken) =
    isSingleLineComment(ft.right) && ft.noBreak

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
      //
      // Predef.augmentString = work around scala/bug#11125 on JDK 11
      augmentString(lit.syntax).lines.map(_.replaceFirst(" *|", "").length).max
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
    case Ident(name) => !name.last.isLetterOrDigit
    case _ => false
  }

  def isSymbolicIdent(tok: Token): Boolean = tok match {
    case Ident(name) => isSymbolicName(name)
    case _ => false
  }

  def isSymbolicName(name: String): Boolean = {
    val head = name.head
    // DESNOTE(2017-02-03, pjrt) Variable names can start with a letter or
    // an `_`, operators cannot. This check should suffice.
    !head.isLetter && head != '_'
  }

  // According to spec:
  // "Operators ending in a colon `:' are right-associative. All other operators are left-associative."
  // See https://www.scala-lang.org/files/archive/spec/2.11/06-expressions.html
  def isRightAssociativeOperator(op: String): Boolean = op.endsWith(":")

  val formatOnCode = Set(
    "// @formatter:on", // IntelliJ
    "// format: on" // scalariform
  )

  def shouldBreak(ft: FormatToken)(implicit style: ScalafmtConfig): Boolean =
    style.newlines.source match {
      case Newlines.classic | Newlines.keep => ft.hasBreak
      case Newlines.fold => false
      case Newlines.unfold => true
    }

}
