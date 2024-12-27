package org.scalafmt.config

import metaconfig._

/** @param includeRegex
  *   Regexp for which infix operators should indent by 2 spaces. For example,
  *   `.*=` produces this output
  *   {{{
  *   a &&
  *   b
  *
  *   a +=
  *     b
  *   }}}
  *
  * @param excludeRegex
  *   Regexp for which infix operators should not indent by 2 spaces. For
  *   example, when [[includeRegex]] is `.*` and [[excludeRegex]] is `&&`
  *   {{{
  *   a &&
  *   b
  *
  *   a ||
  *     b
  *
  *   a +=
  *     b
  *   }}}
  *
  * @param exemptScope
  *   If topLevel, allows no indentation on infix operators in top-level
  *   functions only. For example,
  *   {{{
  *   // top-level, flag doesn't apply
  *   a &&
  *   b
  *   // true
  *   function(
  *     a &&
  *       b
  *   )
  *   // false
  *   function(
  *     a &&
  *     b
  *   )
  *   }}}
  *
  * @see
  *   For context:
  *   [[https://github.com/scala-js/scala-js/blob/master/CODINGSTYLE.md#long-expressions-with-binary-operators]]
  */
case class IndentOperator(
    exemptScope: IndentOperator.Exempt = IndentOperator.Exempt.oldTopLevel,
    @annotation.ExtraName("include")
    includeRegex: String = ".*",
    @annotation.ExtraName("exclude")
    excludeRegex: String = "^(&&|\\|\\|)$",
) {
  private val includeRegexp = includeRegex.r.pattern
  private val excludeRegexp = excludeRegex.r.pattern

  def noindent(op: String): Boolean = excludeRegexp.matcher(op).find() ||
    !includeRegexp.matcher(op).find()
}

object IndentOperator {
  private val default = IndentOperator()
  private val akka = IndentOperator(includeRegex = "^.*=$", excludeRegex = "^$")

  implicit lazy val surface: generic.Surface[IndentOperator] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[IndentOperator] = generic.deriveEncoder

  implicit val decoder: ConfDecoderEx[IndentOperator] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "indentOperator") {
      case Conf.Str("spray" | "akka" | "akka-http") => IndentOperator.akka
      case Conf.Str("default") => IndentOperator.default
    }.withSectionRenames(
      // deprecated since v3.4.0
      annotation.SectionRename { case Conf.Bool(value) =>
        Conf.Str(if (value) "oldTopLevel" else "all")
      }("topLevelOnly", "exemptScope"),
    )

  sealed abstract class Exempt
  object Exempt {
    case object all extends Exempt
    case object oldTopLevel extends Exempt
    case object aloneEnclosed extends Exempt
    case object aloneArgOrBody extends Exempt
    case object notAssign extends Exempt
    case object notWithinAssign extends Exempt

    implicit val reader: ConfCodecEx[Exempt] = ReaderUtil.oneOf[Exempt](
      all,
      oldTopLevel,
      aloneEnclosed,
      aloneArgOrBody,
      notAssign,
      notWithinAssign,
    )
  }

  val boolToAssign: PartialFunction[Conf, Conf] = { case Conf.Bool(value) =>
    if (value) Conf.Obj(
      "exemptScope" -> Conf.Str("notAssign"),
      "excludeRegex" -> Conf.Str(".*"),
    )
    else Conf.Obj.empty
  }
}
