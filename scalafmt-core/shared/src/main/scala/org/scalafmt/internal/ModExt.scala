package org.scalafmt.internal

import scala.meta.tokens.Token

import scala.language.implicitConversions

/** @param mod
  *   Is this a space, no space, newline or 2 newlines?
  * @param indents
  *   Does this add indentation?
  */
case class ModExt(mod: Modification, indents: Seq[Indent] = Seq.empty) {
  lazy val indentation = indents.mkString("[", ", ", "]")

  @inline
  def isNL: Boolean = mod.isNL

  def withIndent(length: => Length, expire: => Token, when: ExpiresOn): ModExt =
    length match {
      case Length.Num(0, _) => this
      case x => withIndentImpl(Indent(x, expire, when))
    }

  def withIndentOpt(
      length: => Length,
      expire: Option[Token],
      when: ExpiresOn,
  ): ModExt = expire.fold(this)(withIndent(length, _, when))

  def withIndent(indent: => Indent): ModExt = indent match {
    case Indent.Empty => this
    case x => withIndentImpl(x)
  }

  def withIndentOpt(indent: => Option[Indent]): ModExt = indent
    .fold(this)(withIndent(_))

  def withIndents(indents: Seq[Indent]): ModExt = indents
    .foldLeft(this)(_ withIndent _)

  private def withIndentImpl(indent: Indent): ModExt =
    copy(indents = indent +: indents)

  def switch(trigger: Token, on: Boolean): ModExt = {
    val newIndents = indents.flatMap { x =>
      Some(x.switch(trigger, on)).filter(_ ne Indent.Empty)
    }
    copy(indents = newIndents)
  }

  /** This gap is necessary for pretty alignment multiline expressions on the
    * right-side of enumerator. Without:
    * ```
    * for {
    *   a <- new Integer {
    *         value = 1
    *       }
    *   x <- if (variable) doSomething
    *       else doAnything
    * }
    * ```
    *
    * With:
    * ```
    * for {
    *   a <- new Integer {
    *           value = 1
    *        }
    *   x <- if (variable) doSomething
    *        else doAnything
    * }
    * ```
    */
  def getActualIndents(offset: Int): Seq[ActualIndent] = indents
    .flatMap(_.withStateOffset(offset + mod.length))

}

object ModExt {

  implicit def implicitModToModExt(mod: Modification): ModExt = ModExt(mod)

}
