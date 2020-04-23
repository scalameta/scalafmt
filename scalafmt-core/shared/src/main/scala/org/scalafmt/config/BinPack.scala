package org.scalafmt.config

import metaconfig._

/**
  *
  * @param unsafeCallSite      DO NOT USE! This option is buggy for complicated expressions.
  *                            The only reason this option exists is to support
  *                            the [[literalArgumentLists]] option, which enables callSite
  *                            only for simple expressions.
  *
  *                            If true, will fit as many arguments on each line,
  *                            only breaking at commas. If false, a function
  *                            call's arguments will either be all on the same
  *                            line or will have one line each.
  * @param unsafeDefnSite            Same as [[unsafeCallSite]], except for definition site.
  * @param literalArgumentLists
  *                            If true, automatically sets the style to bin-pack for argument lists
  *                            that only consist of literals.
  * @param literalsMinArgCount Argument list must be longer than this setting
  *                            to be eligible for [[literalArgumentLists]].
  * @param literalsInclude     Regexes for literal type names. For example, "Int"
  *                            or "Byte".
  * @param literalsExclude     Regexes for literal to exclude from [[literalArgumentLists]].
  * @param parentConstructors  Parent constructors are C and D in
  *                            "class A extends B with C and D". If true,
  *                            scalafmt will fit as many parent constructors
  *                            on a single line. If false, each parent
  *                            constructor gets its own line.
  *
  */
case class BinPack(
    unsafeCallSite: Boolean = false,
    unsafeDefnSite: Boolean = false,
    parentConstructors: Boolean = false,
    literalArgumentLists: Boolean = true,
    literalsSingleLine: Boolean = false,
    literalsMinArgCount: Int = 5,
    literalsInclude: Seq[String] = Seq(".*"),
    literalsExclude: Seq[String] = Seq("String", "Term.Name")
) extends Decodable[BinPack] {
  override protected[config] def baseDecoder =
    generic.deriveDecoder(this).noTypos
  def literalsRegex: FilterMatcher =
    FilterMatcher(literalsInclude, literalsExclude)
}
object BinPack {
  implicit lazy val surface: generic.Surface[BinPack] =
    generic.deriveSurface[BinPack]
  implicit lazy val encoder: ConfEncoder[BinPack] = generic.deriveEncoder

  val enabled = BinPack(
    unsafeDefnSite = true,
    unsafeCallSite = true,
    parentConstructors = true
  )
  implicit val preset: PartialFunction[Conf, BinPack] = {
    case Conf.Bool(true) => enabled
    case Conf.Bool(false) => BinPack()
  }

}
