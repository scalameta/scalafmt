package org.scalafmt.config

import scala.meta.tokens.Token

import metaconfig._

/** @param unsafeCallSite
  *   DO NOT USE! This option is buggy for complicated expressions. The only
  *   reason this option exists is to support the [[literalArgumentLists]]
  *   option, which enables callSite only for simple expressions.
  *
  * If true, will fit as many arguments on each line, only breaking at commas.
  * If false, a function call's arguments will either be all on the same line or
  * will have one line each.
  * @param unsafeDefnSite
  *   Same as [[unsafeCallSite]], except for definition site.
  * @param literalArgumentLists
  *   If true, automatically sets the style to bin-pack for argument lists that
  *   only consist of literals.
  * @param literalsMinArgCount
  *   Argument list must be longer than this setting to be eligible for
  *   [[literalArgumentLists]].
  * @param literalsInclude
  *   Regexes for literal type names. For example, "Int" or "Byte".
  * @param literalsExclude
  *   Regexes for literal to exclude from [[literalArgumentLists]].
  * @param parentConstructors
  *   Parent constructors are C and D in "class A extends B with C and D". If
  *   "Always", scalafmt will fit as many parent constructors on a single line.
  *   If "Never", each parent constructor gets its own line.
  */
case class BinPack(
    unsafeCallSite: BinPack.Unsafe = BinPack.Unsafe.Never,
    unsafeDefnSite: BinPack.Unsafe = BinPack.Unsafe.Never,
    private val bracketCallSite: Option[BinPack.Unsafe] = None,
    private val bracketDefnSite: Option[BinPack.Unsafe] = None,
    indentCallSiteOnce: Boolean = false,
    indentCallSiteSingleArg: Boolean = true,
    parentConstructors: BinPack.ParentCtors = BinPack.ParentCtors.source,
    literalArgumentLists: Boolean = true,
    literalsIncludeSimpleExpr: Boolean = false,
    literalsSingleLine: Boolean = false,
    literalsMinArgCount: Int = 5,
    literalsInclude: Seq[String] = Seq(".*"),
    literalsExclude: Seq[String] = Seq("String", "Term.Name")
) {
  def literalsRegex: FilterMatcher =
    FilterMatcher(literalsInclude, literalsExclude)

  def keepParentConstructors(implicit style: ScalafmtConfig): Boolean =
    parentConstructors.eq(BinPack.ParentCtors.keep) ||
      style.newlines.source.eq(Newlines.keep) &&
      parentConstructors.eq(BinPack.ParentCtors.source)

  @inline def callSite(open: Token): BinPack.Unsafe =
    callSite(open.is[Token.LeftBracket])
  def callSite(isBracket: Boolean): BinPack.Unsafe =
    (if (isBracket) bracketCallSite else None).getOrElse(unsafeCallSite)

  @inline def defnSite(open: Token): BinPack.Unsafe =
    defnSite(open.is[Token.LeftBracket])
  def defnSite(isBracket: Boolean): BinPack.Unsafe =
    (if (isBracket) bracketDefnSite else None).getOrElse(unsafeDefnSite)

}

object BinPack {
  implicit lazy val surface: generic.Surface[BinPack] =
    generic.deriveSurface[BinPack]
  implicit lazy val encoder: ConfEncoder[BinPack] = generic.deriveEncoder

  val enabled = BinPack(
    unsafeDefnSite = Unsafe.Always,
    unsafeCallSite = Unsafe.Always,
    parentConstructors = ParentCtors.Always
  )

  implicit val decoder =
    Presets.mapDecoder(generic.deriveDecoderEx(BinPack()).noTypos, "binPack") {
      case Conf.Bool(true) => enabled
      case Conf.Bool(false) => BinPack()
    }

  sealed abstract class ParentCtors
  object ParentCtors {
    case object source extends ParentCtors
    case object keep extends ParentCtors
    case object Always extends ParentCtors
    case object Never extends ParentCtors
    case object Oneline extends ParentCtors
    case object OnelineIfPrimaryOneline extends ParentCtors

    implicit val oneOfReader: ConfCodecEx[ParentCtors] =
      ReaderUtil.oneOfCustom[ParentCtors](
        source,
        keep,
        Always,
        Never,
        Oneline,
        OnelineIfPrimaryOneline
      ) {
        case Conf.Bool(true) => Configured.ok(Always)
        case Conf.Bool(false) => Configured.ok(Never)
      }

  }

  sealed abstract class Unsafe {
    final def isNever: Boolean = this eq Unsafe.Never
  }
  object Unsafe {
    case object Never extends Unsafe
    case object Always extends Unsafe
    case object Oneline extends Unsafe

    implicit val oneOfReader: ConfCodecEx[Unsafe] =
      ReaderUtil.oneOfCustom[Unsafe](Never, Always, Oneline) {
        case Conf.Bool(true) => Configured.ok(Always)
        case Conf.Bool(false) => Configured.ok(Never)
      }
  }

}
