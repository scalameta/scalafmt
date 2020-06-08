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
      case Length.Num(0) => this
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
    copy(indents = indent +: indents)

  def switch(switchObject: AnyRef): ModExt = {
    val newIndents = indents.map(_.switch(switchObject))
    copy(indents = newIndents.filter(_ ne Indent.Empty))
  }

}

object ModExt {

  implicit def implicitModToModExt(mod: Modification): ModExt = ModExt(mod)

}
