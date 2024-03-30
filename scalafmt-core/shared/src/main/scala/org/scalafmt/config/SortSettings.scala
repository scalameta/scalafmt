package org.scalafmt.config

import metaconfig._
import scala.meta.Mod
import sourcecode.Text

import scala.meta.classifiers.Classifier

case class SortSettings(order: List[SortSettings.ModKey])

object SortSettings {
  final case class ModKey(name: String, matches: Mod => Boolean)

  object ModKey {
    def apply[A](name: String)(implicit
        classifier: Classifier[Mod, A]
    ): ModKey = ModKey(name, _.is[A])
  }

  private val modImplicit = ModKey[Mod.Implicit]("implicit")
  private val modErased = ModKey[Mod.Erased]("erased")
  private val modFinal = ModKey[Mod.Final]("final")
  private val modSealed = ModKey[Mod.Sealed]("sealed")
  private val modAbstract = ModKey[Mod.Abstract]("abstract")
  private val modOverride = ModKey[Mod.Override]("override")
  private val modPrivate = ModKey[Mod.Private]("private")
  private val modProtected = ModKey[Mod.Protected]("protected")
  private val modLazy = ModKey[Mod.Lazy]("lazy")
  private val modOpen = ModKey[Mod.Open]("open")
  private val modTransparent = ModKey[Mod.Transparent]("transparent")
  private val modInline = ModKey[Mod.Inline]("inline")
  private val modInfix = ModKey[Mod.Infix]("infix")
  private val modOpaque = ModKey[Mod.Opaque]("opaque")

  private val defaultOrder: List[ModKey] = List(
    // implicit
    modImplicit,
    // final
    modFinal,
    modSealed,
    modAbstract,
    // override
    modOverride,
    // access
    modPrivate,
    modProtected,
    // other
    modErased,
    modLazy,
    modOpen,
    modTransparent,
    modInline,
    modInfix,
    modOpaque
  )

  // https://docs.scala-lang.org/style/declarations.html#modifiers
  private def styleGuideOrder: List[ModKey] = List(
    // override
    modOverride,
    // access
    modPrivate,
    modProtected,
    // implicit
    modImplicit,
    // final
    modFinal,
    modSealed,
    modAbstract,
    // other
    modErased,
    modLazy,
    modOpen,
    modTransparent,
    modInline,
    modInfix,
    modOpaque
  )

  implicit val sortSettingsModKeyCodec: ConfCodecEx[ModKey] = ReaderUtil
    .oneOf[ModKey](defaultOrder.map(v => Text(v, v.name)): _*)

  implicit val surface: generic.Surface[SortSettings] = generic.deriveSurface

  // NOTE: Originally, the configuration parser failed if a modifier was
  // missing from the configuration but this behavior was problematic
  // because it was a breaking change to add formatting support for a
  // new modifier like Scala 3 "open". Instead, modifiers with no configuration
  // get sorted to the front of the list.
  val default: SortSettings = SortSettings(defaultOrder)

  private val styleGuide = SortSettings(styleGuideOrder)

  private implicit val preset: PartialFunction[Conf, SortSettings] = {
    case Conf.Str("default") => default
    case Conf.Str("styleGuide") => styleGuide
  }

  implicit val encoder: ConfEncoder[SortSettings] = generic.deriveEncoder

  implicit final val decoder: ConfDecoderEx[SortSettings] = Presets
    .mapDecoder(generic.deriveDecoderEx(default).noTypos, "sortModifiers")

}
