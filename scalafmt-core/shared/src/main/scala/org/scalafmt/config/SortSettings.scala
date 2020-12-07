package org.scalafmt.config

import metaconfig._
import scala.meta.classifiers.Classifier
import scala.meta.Tree
import scala.meta.Mod
import sourcecode.Name
import sourcecode.Text

case class SortSettings(
    order: List[SortSettings.ModKey]
)

object SortSettings {
  final case class ModKey(
      name: String,
      matches: Mod => Boolean
  ) extends Product
  private def createMod[T <: Mod](implicit
      name: Name,
      c: Classifier[Tree, T]
  ): ModKey = ModKey(name.value, m => c(m))
  val `private` = createMod[Mod.Private]
  val `protected` = createMod[Mod.Protected]
  val `final` = createMod[Mod.Final]
  val `sealed` = createMod[Mod.Sealed]
  val `abstract` = createMod[Mod.Abstract]
  val `implicit` = createMod[Mod.Implicit]
  val `override` = createMod[Mod.Override]
  val `lazy` = createMod[Mod.Lazy]
  val `open` = createMod[Mod.Open]

  val defaultOrder: List[ModKey] = List(
    `implicit`,
    //
    `final`,
    `sealed`,
    `abstract`,
    //
    `override`,
    //
    `private`,
    `protected`,
    //
    `lazy`,
    `open`
  )

  implicit val SortSettingsModKeyReader: ConfCodec[ModKey] =
    ReaderUtil.oneOf[ModKey](defaultOrder.map(v => Text(v, v.name)): _*)

  implicit val surface: generic.Surface[SortSettings] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[SortSettings] =
    generic.deriveEncoder
  implicit val reader: ConfDecoder[SortSettings] =
    // NOTE: Originally, the configuration parser failed if a modifier was
    // missing from the configuration but this behavior was problematic
    // because it was a breaking change to add formatting support for a
    // new modifier like Scala 3 "open". Instead, modifiers with no configuration
    // get sorted to the front of the list.
    generic.deriveDecoder(SortSettings(defaultOrder))

  def default: SortSettings =
    SortSettings(defaultOrder)

}
