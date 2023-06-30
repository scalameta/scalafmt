package org.scalafmt.config

import metaconfig._

/** @param wrap
  *   defines whether to wrap comments; the comment to be wrapped may not
  *   contain nested comments.
  *   - no: do not wrap
  *   - trailing: wrap the last comment on a line (line break after)
  *   - standalone: wrap standalone comments (line break both before and after
  *     the comment)
  * @param wrapStandaloneSlcAsSlc
  *   if `wrap` is enabled, wrap standalone single-line comments (//) using the
  *   same type, rather than multi-line comments (/* ... */); it won't be
  *   applied to trailing comments as indentation would be inconsistent.
  */
case class Comments(
    wrap: Comments.Wrap = Comments.Wrap.no,
    wrapSingleLineMlcAsSlc: Boolean = false,
    wrapStandaloneSlcAsSlc: Boolean = false
) {
  @inline def willWrap: Boolean = wrap ne Comments.Wrap.no
}

object Comments {

  implicit val surface: generic.Surface[Comments] =
    generic.deriveSurface[Comments]
  implicit val codec: ConfCodecEx[Comments] =
    generic.deriveCodecEx(Comments()).noTypos

  sealed abstract class Wrap
  object Wrap {
    case object no extends Wrap
    case object standalone extends Wrap
    case object trailing extends Wrap
    implicit val reader: ConfCodecEx[Wrap] =
      ReaderUtil.oneOf[Wrap](no, standalone, trailing)
  }

}
