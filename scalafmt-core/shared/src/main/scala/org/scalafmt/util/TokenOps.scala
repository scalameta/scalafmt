package org.scalafmt.util

import scala.meta.classifiers.Classifier
import scala.meta.{Defn, Pkg, Source, Template, Term, Tree}
import scala.meta.tokens.Token
import scala.meta.tokens.Token._
import scala.meta.tokens.Tokens

import org.scalafmt.config.Newlines
import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal.FormatToken
import org.scalafmt.internal.Modification
import org.scalafmt.internal.NewlineT
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

  def isDocstring(text: String): Boolean =
    text.lengthCompare(4) > 0 && text.startsWith("/**") // excludes /**/

  def blankLineBeforeDocstring(
      ft: FormatToken
  )(implicit style: ScalafmtConfig): Boolean =
    style.optIn.forceNewlineBeforeDocstringSummary &&
      ft.right.is[Token.Comment] && !ft.left.is[Token.Comment] &&
      isDocstring(ft.meta.right.text) &&
      TreeOps
        .findTreeOrParent(ft.meta.leftOwner) {
          case t if t.pos.end <= ft.right.start => None
          case _: Pkg | _: Source | _: Template | _: Term.Block => Some(false)
          case _ => Some(true)
        }
        .isEmpty

  // 2.13 implements SeqOps.findLast
  def findLast[A](seq: Seq[A])(cond: A => Boolean): Option[A] =
    seq.reverseIterator.find(cond)

  def lastTokenOpt(tokens: Tokens): Option[Token] =
    findLast(tokens) {
      case Trivia() | _: EOF => false
      case _ => true
    }

  def lastToken(tokens: Tokens): Token =
    lastTokenOpt(tokens).getOrElse(tokens.last)

  def lastToken(tree: Tree): Token = lastToken(tree.tokens)

  def endsWithNoIndent(between: Seq[Token]): Boolean =
    between.lastOption.exists(_.is[LF])

  def rhsIsCommentedOut(formatToken: FormatToken): Boolean =
    isSingleLineComment(formatToken.right) &&
      endsWithNoIndent(formatToken.between)

  val booleanOperators = Set("&&", "||")

  def isBoolOperator(token: Token): Boolean =
    booleanOperators.contains(token.syntax)

  def identModification(ident: Ident): Modification = {
    val lastCharacter = ident.syntax.last
    Space(!Character.isLetterOrDigit(lastCharacter) && lastCharacter != '`')
  }

  @inline
  def isSingleLineComment(c: String): Boolean = c.startsWith("//")

  @inline
  def isSingleLineComment(c: Token.Comment): Boolean = {
    (c.end - c.start) >= 2 &&
    c.input.chars(c.start) == '/' &&
    c.input.chars(c.start + 1) == '/'
  }

  def isSingleLineComment(token: Token): Boolean =
    token match {
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

  def defnTemplate(tree: Tree): Option[Template] =
    tree match {
      case t: Defn.Object => Some(t.templ)
      case t: Defn.Class => Some(t.templ)
      case t: Defn.Trait => Some(t.templ)
      case t: Pkg.Object => Some(t.templ)
      case t: Template => Some(t)
      case _ => None
    }

  def defnBeforeTemplate(tree: Tree): Option[Tree] =
    tree match {
      case t: Defn.Object => Some(t.name)
      case t: Defn.Class => Some(t.ctor)
      case t: Defn.Trait => Some(t.ctor)
      case t: Pkg.Object => Some(t.name)
      case _ => None
    }

  val formatOnCode = Set(
    "@formatter:on", // IntelliJ
    "format: on" // scalariform
  )

  val formatOffCode = Set(
    "@formatter:off", // IntelliJ
    "format: off" // scalariform
  )

  @inline
  def isFormatOn(token: Token): Boolean = isFormatIn(token, formatOnCode)

  @inline
  def isFormatOff(token: Token): Boolean = isFormatIn(token, formatOffCode)

  private def isFormatIn(token: Token, set: Set[String]): Boolean =
    token match {
      case t: Comment => set.contains(t.value.trim.toLowerCase)
      case _ => false
    }

  def endsWithSymbolIdent(tok: Token): Boolean =
    tok match {
      case Ident(name) => !name.last.isLetterOrDigit
      case _ => false
    }

  def isSymbolicIdent(tok: Token): Boolean =
    tok match {
      case Ident(name) => isSymbolicName(name)
      case _ => false
    }

  def isSymbolicName(name: String): Boolean = {
    val head = name.head
    // DESNOTE(2017-02-03, pjrt) Variable names can start with a letter or
    // an `_`, operators cannot. This check should suffice.
    !head.isLetter && head != '_'
  }

  def shouldBreak(ft: FormatToken)(implicit style: ScalafmtConfig): Boolean =
    style.newlines.source match {
      case Newlines.classic | Newlines.keep => ft.hasBreak
      case Newlines.fold => false
      case Newlines.unfold => true
    }

  def classifyOnRight[A](cls: Classifier[Token, A])(ft: FormatToken): Boolean =
    cls(ft.right)

  def findArgsFor[A <: Tree](
      token: Token,
      argss: Seq[Seq[A]],
      matching: Map[TokenHash, Token]
  ): Option[Seq[A]] =
    matching.get(hash(token)).flatMap { other =>
      // find the arg group starting with given format token
      val beg = math.min(token.start, other.start)
      argss
        .find(_.headOption.exists(_.tokens.head.start >= beg))
        .filter(_.head.tokens.head.start <= math.max(token.end, other.end))
    }

  def getXmlLastLineIndent(tok: Xml.Part): Option[Int] = {
    val part = tok.value
    val afterLastNL = part.lastIndexOf('\n') + 1
    if (afterLastNL <= 0) None
    else {
      val nonWs = part.indexWhere(!_.isWhitespace, afterLastNL)
      Some((if (nonWs < 0) part.length else nonWs) - afterLastNL)
    }
  }

}
