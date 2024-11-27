package org.scalafmt.internal

import scala.meta.tokens.{Token => T}

import scala.annotation.tailrec

/** @param mod
  *   Is this a space, no space, newline or 2 newlines?
  * @param indents
  *   Does this add indentation?
  */
case class ModExt(
    mod: Modification,
    indents: Seq[Indent] = Nil,
    altOpt: Option[ModExt] = None,
    noAltIndent: Boolean = false,
) {
  @inline
  def isNL: Boolean = mod.isNL

  @tailrec
  private def toString(prefix: String, indentPrefix: String): String = {
    @inline
    def res(suffix: String) = {
      val ind = if (indents.isEmpty) "" else indents.mkString("[", ", ", "]")
      s"$prefix$mod$indentPrefix$ind$suffix"
    }

    altOpt match {
      case None => res("")
      case Some(x) => x.toString(res("|"), if (noAltIndent) "" else "+")
    }
  }

  override def toString: String = toString("", "")

  def withAlt(alt: => ModExt, noAltIndent: Boolean = false): ModExt =
    copy(altOpt = Some(alt), noAltIndent = noAltIndent)

  @inline
  def withAltIf(
      ok: Boolean,
  )(alt: => ModExt, noAltIndent: Boolean = false): ModExt =
    if (ok) withAlt(alt, noAltIndent = noAltIndent) else this

  def orMod(flag: Boolean, mod: => ModExt): ModExt = if (flag) this else mod

  def withIndent(length: => Length, expire: => FT, when: ExpiresOn): ModExt =
    length match {
      case Length.Num(0, _) => this
      case x => withIndentImpl(Indent(x, expire, when))
    }

  def withIndentOpt(
      length: => Length,
      expire: Option[FT],
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

  def switch(trigger: T, on: Boolean): ModExt = {
    val newIndents = indents
      .flatMap(x => Some(x.switch(trigger, on)).filter(_ ne Indent.Empty))
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
