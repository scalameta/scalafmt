package org.scalafmt.config

import scala.meta._
import scala.meta.tokens.Token

import metaconfig._

/** @param callSite
  *   If true, will fit as many arguments on each line, only breaking at commas.
  *   If false, a function call's arguments will either be all on the same line
  *   or will have one line each.
  * @param defnSite
  *   Same as [[callSite]], except for definition site.
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
    @annotation.ExtraName("unsafeCallSite")
    callSite: BinPack.Site = BinPack.Site.Never,
    @annotation.ExtraName("unsafeDefnSite")
    defnSite: BinPack.Site = BinPack.Site.Never,
    private val bracketCallSite: Option[BinPack.Site] = None,
    bracketDefnSite: Option[BinPack.Site] = None,
    indentCallSiteOnce: Boolean = false,
    indentCallSiteSingleArg: Boolean = true,
    parentConstructors: BinPack.ParentCtors = BinPack.ParentCtors.source,
    literalArgumentLists: Boolean = true,
    literalsIncludeSimpleExpr: Boolean = false,
    literalsSingleLine: Boolean = false,
    literalsMinArgCount: Int = 5,
    literalsInclude: Seq[String] = Seq(".*"),
    literalsExclude: Seq[String] = Seq("String", "Term.Name"),
) {
  def literalsRegex: FilterMatcher =
    FilterMatcher(literalsInclude, literalsExclude)

  def keepParentConstructors(implicit style: ScalafmtConfig): Boolean =
    parentConstructors.eq(BinPack.ParentCtors.keep) ||
      style.newlines.source.eq(Newlines.keep) &&
      parentConstructors.eq(BinPack.ParentCtors.source)

  @inline
  def callSiteFor(open: Token): BinPack.Site =
    callSiteFor(open.is[Token.LeftBracket])
  def callSiteFor(isBracket: Boolean): BinPack.Site =
    (if (isBracket) bracketCallSite else None).getOrElse(callSite)
  def callSiteFor(owner: Tree): BinPack.Site =
    callSiteFor(owner.is[Type.ArgClause])

  @inline
  def defnSiteFor(open: Token): BinPack.Site =
    defnSiteFor(open.is[Token.LeftBracket])
  def defnSiteFor(isBracket: Boolean): BinPack.Site =
    (if (isBracket) bracketDefnSite else None).getOrElse(defnSite)
  def defnSiteFor(owner: Tree): BinPack.Site =
    defnSiteFor(owner.is[Type.ParamClause])

}

object BinPack {
  implicit lazy val surface: generic.Surface[BinPack] = generic
    .deriveSurface[BinPack]
  implicit lazy val encoder: ConfEncoder[BinPack] = generic.deriveEncoder

  def ctor(site: Site, parents: ParentCtors): BinPack =
    BinPack(defnSite = site, callSite = site, parentConstructors = parents)

  val never = BinPack.ctor(Site.Never, ParentCtors.Never)
  val always = BinPack.ctor(Site.Always, ParentCtors.Always)
  private val oneline = BinPack.ctor(Site.Oneline, ParentCtors.Oneline)

  private val customPresets = ReaderUtil.Custom(never, always, oneline)

  implicit val decoder: ConfDecoderEx[BinPack] = Presets
    .mapDecoder(generic.deriveDecoderEx(never).noTypos, "binPack") {
      case Conf.Bool(true) => always
      case Conf.Bool(false) => never
      case Conf.Str(customPresets(obj)) => obj
    }

  sealed abstract class ParentCtors
  object ParentCtors {
    case object source extends ParentCtors
    case object keep extends ParentCtors
    case object Always extends ParentCtors
    case object Never extends ParentCtors
    case object Oneline extends ParentCtors
    case object OnelineIfPrimaryOneline extends ParentCtors

    implicit val oneOfReader: ConfCodecEx[ParentCtors] = ReaderUtil
      .oneOfCustom[ParentCtors](
        source,
        keep,
        Always,
        Never,
        Oneline,
        OnelineIfPrimaryOneline,
      ) {
        case Conf.Bool(true) => Configured.ok(Always)
        case Conf.Bool(false) => Configured.ok(Never)
      }

  }

  sealed abstract class Site
  object Site {
    case object Never extends Site
    case object Always extends Site
    case object Oneline extends Site

    implicit val oneOfReader: ConfCodecEx[Site] = ReaderUtil
      .oneOfCustom[Site](Never, Always, Oneline) {
        case Conf.Bool(true) => Configured.ok(Always)
        case Conf.Bool(false) => Configured.ok(Never)
      }
  }

}
