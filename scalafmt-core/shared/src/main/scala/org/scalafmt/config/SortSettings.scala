package org.scalafmt.config

import metaconfig._
import scala.meta.Mod
import sourcecode.Text

case class SortSettings(
    order: List[SortSettings.ModKey]
)

object SortSettings {
  final case class ModKey(
      name: String,
      matches: Mod => Boolean
  )

  val defaultOrder: List[ModKey] = List(
    ModKey("implicit", _.is[Mod.Implicit]),
    //
    ModKey("final", _.is[Mod.Final]),
    ModKey("sealed", _.is[Mod.Sealed]),
    ModKey("abstract", _.is[Mod.Abstract]),
    //
    ModKey("override", _.is[Mod.Override]),
    //
    ModKey("private", _.is[Mod.Private]),
    ModKey("protected", _.is[Mod.Protected]),
    //
    ModKey("lazy", _.is[Mod.Lazy]),
    ModKey("open", _.is[Mod.Open])
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
