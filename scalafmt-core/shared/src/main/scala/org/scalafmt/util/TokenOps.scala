package org.scalafmt.util

import org.scalafmt.config.ScalafmtConfig
import org.scalafmt.internal._

import scala.meta._
import scala.meta.tokens.Token.{Space => _, _}
import scala.meta.tokens.Tokens
import scala.meta.tokens.{Token => T}

/** Stateless helper functions on [[scala.meta.Token]].
  */
object TokenOps {

  /** For convenience when experimenting with different hashing strategies.
    */
  type TokenHash = Long

  /** Custom hash code for token.
    *
    * The default hashCode is slow because it prevents conflicts between tokens
    * from different source files. We only care about getting a unique
    * identifier for the token inside this source file.
    *
    * The hash code works like this this:
    *   - Top 8 bits go to a hashCode of productPrefix, a unique identifier for
    *     the tokens class
    *   - Next 28 bits go to the tokens **start** offset byte
    *   - Final 28 bits go to the tokens **end** offset byte.
    *
    * The only chance for collision is if two empty length tokens with the same
    * type lie next to each other. @xeno-by said this should not happen.
    */
  @inline
  def hash(token: T): TokenHash = {
    val longHash: Long = token.productPrefix.hashCode.toLong << 62 - 8 |
      token.start.toLong << 62 - (8 + 28) | token.end
    longHash
  }

  def isDocstring(text: String): Boolean = text.length > 4 &&
    text.startsWith("/**") // excludes /**/

  def blankLineBeforeDocstring(ft: FT)(implicit
      style: ScalafmtConfig,
  ): Boolean = style.docstrings.forceBlankLineBefore &&
    isDocstring(ft.meta.right.text) &&
    // we need Pkg in case docstring comes before the first statement and not owned by Pkg.Body
    ft.meta.rightOwner.isAny[Pkg, Tree.Block]

  // 2.13 implements SeqOps.findLast
  def findLast[A](seq: Seq[A])(cond: A => Boolean): Option[A] = seq
    .reverseIterator.find(cond)

  def findLastVisibleTokenOpt(tokens: Tokens): Option[T] = findLast(tokens) {
    case _: Whitespace | _: EOF => false
    case _ => true
  }

  def findLastVisibleToken(tokens: Tokens): T = findLastVisibleTokenOpt(tokens)
    .getOrElse(tokens.last)

  @inline
  def withNoIndent(ft: FT): Boolean = ft.between.lastOption.is[AtEOL]

  @inline
  def rhsIsCommentedOut(ft: FT): Boolean = ft.right.is[Comment] &&
    rhsIsCommentedOutIfComment(ft)

  @inline
  def rhsIsCommentedOutIfComment(ft: FT): Boolean = withNoIndent(ft) &&
    isSingleLineIfComment(ft.right)

  @inline
  def isLeftCommentThenBreak(ft: FT): Boolean = ft.left.is[Comment] &&
    ft.hasBreak

  def isSingleLineIfComment(c: T): Boolean = {
    val off = c.start
    c.end - off >= 2 && {
      val chars = c.input.chars
      chars(off) == '/' && chars(off + 1) == '/'
    }
  }

  val booleanOperators = Set("&&", "||")

  def isBoolOperator(token: T): Boolean = booleanOperators.contains(token.syntax)

  def identModification(ident: Ident): Modification = {
    val lastCharacter = ident.syntax.last
    Space(!Character.isLetterOrDigit(lastCharacter) && lastCharacter != '`')
  }

  @inline
  def getMod(ft: FT): Modification = Space.orNL(ft.newlinesBetween)

  def endsWithSymbolIdent(tok: T): Boolean = tok match {
    case Ident(name) => !name.last.isLetterOrDigit && !tok.isBackquoted
    case _ => false
  }

  def isSymbolicIdent(tok: T): Boolean = tok match {
    case Ident(name) => isSymbolicName(name)
    case _ => false
  }

  def isSymbolicName(name: String): Boolean = {
    val head = name.head
    // DESNOTE(2017-02-03, pjrt) Variable names can start with a letter or
    // an `_`, operators cannot. This check should suffice.
    !head.isLetter && head != '_'
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

  def getIndentTrigger(tree: Tree): T = tree.tokens.head

}
