package org.scalafmt.internal

import scala.language.implicitConversions
import scala.meta.tokens.Token

/**
  * @param mod Is this a space, no space, newline or 2 newlines?
  * @param indents Does this add indentation?
  */
case class ModExt(
    mod: Modification,
    indents: Seq[Indent] = Seq.empty
) {
  lazy val indentation = indents.mkString("[", ", ", "]")

  def withIndent(length: => Length, expire: => Token, when: ExpiresOn): ModExt =
    length match {
      case Length.Num(0, _) => this
      case x => withIndentImpl(Indent(x, expire, when))
    }

  def withIndentOpt(
      length: => Length,
      expire: Option[Token],
      when: ExpiresOn
  ): ModExt =
    expire.fold(this)(withIndent(length, _, when))

  def withIndent(indent: => Indent): ModExt =
    indent match {
      case Indent.Empty => this
      case x => withIndentImpl(x)
    }

  def withIndentOpt(indent: => Option[Indent]): ModExt =
    indent.fold(this)(withIndent(_))

  def withIndents(indents: Seq[Indent]): ModExt =
    indents.foldLeft(this)(_ withIndent _)

  private def withIndentImpl(indent: Indent): ModExt =
    copy(indents = indents :+ indent)

  def switch(trigger: Token): ModExt = {
    val newIndents = indents.map(_.switch(trigger))
    copy(indents = newIndents.filter(_ ne Indent.Empty))
  }

  /**
    * This gap is necessary for pretty alignment multiline expressions
    * on the right-side of enumerator.
    * Without:
    * ```
    * for {
    *    a <- new Integer {
    *          value = 1
    *        }
    *   x <- if (variable) doSomething
    *       else doAnything
    * }
    * ```
    *
    * With:
    * ```
    * for {
    *    a <- new Integer {
    *           value = 1
    *         }
    *   x <- if (variable) doSomething
    *        else doAnything
    * }
    * ```
    */
  def getActualIndents(offset: Int): Seq[ActualIndent] = {
    val adjustedOffset = if (mod eq Space) offset + 1 else offset
    indents.flatMap(_.withStateOffset(adjustedOffset))
  }

}

object ModExt {

  implicit def implicitModToModExt(mod: Modification): ModExt = ModExt(mod)

}
