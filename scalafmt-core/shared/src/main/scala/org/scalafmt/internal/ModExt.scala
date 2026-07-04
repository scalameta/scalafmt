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
    indents: List[Indent] = Nil,
    alt: ModExt = null,
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

    if (alt eq null) res("")
    else alt.toString(res("|"), if (noAltIndent) "" else "+")
  }

  override def toString: String = toString("", "")

  def withAlt(alt: => ModExt, noAltIndent: Boolean = false): ModExt =
    copy(alt = alt, noAltIndent = noAltIndent)

  @inline
  def withAltIf(
      ok: Boolean,
  )(alt: => ModExt, noAltIndent: Boolean = false): ModExt =
    if (ok) withAlt(alt, noAltIndent = noAltIndent) else this

  def orMod(flag: Boolean, mod: => ModExt): ModExt = if (flag) this else mod

  def withIndent(length: => Length, expire: FT, when: ExpiresOn): ModExt =
    if (expire eq null) this
    else length match {
      case Length.Num(0, _) => this
      case x => withIndentImpl(Indent(x, expire, when))
    }

  def withIndent(indent: Indent): ModExt = indent match {
    case Indent.Empty => this
    case x => withIndentImpl(x)
  }

  def withIndents(indents: Seq[Indent]): ModExt = indents
    .foldLeft(this)(_ withIndent _)

  private def withIndentImpl(indent: Indent): ModExt =
    copy(indents = indent +: indents)

  def switch(trigger: T, on: Boolean): ModExt = {
    val newIndents = indents
      .flatMap(x => Some(x.switch(trigger, on)).filter(_ ne Indent.Empty))
    copy(indents = newIndents)
  }

}
