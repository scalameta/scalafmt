package org.scalafmt.config

import metaconfig._

/** @param annotationNewlines
  *   - if newlines.source is missing or keep:
  *     - if true, will keep existing line breaks around annotations
  *   - if newlines.source is fold:
  *     - if true, will break before the entity being annotated
  *     - will not force break between consecutive annotations
  *   - if newlines.source is unfold:
  *     - if true, will break between consecutive annotations
  *     - will always break before the entity being annotated
  *
  * @param selfAnnotationNewline
  *   See https://github.com/scalameta/scalafmt/issues/938 If true, will force a
  *   line break before a self annotation if there was a line break there
  *   before.
  */
case class OptIn(
    selfAnnotationNewline: Boolean = true,
    annotationNewlines: Boolean = true,
)

object OptIn {
  implicit lazy val surface: generic.Surface[OptIn] = generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[OptIn] = generic.deriveCodecEx(OptIn())
    .noTypos
}
