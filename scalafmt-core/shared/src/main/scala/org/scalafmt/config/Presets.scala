package org.scalafmt.config

import metaconfig._

object Presets {

  val presetKey = "preset"

  def mapDecoder[A <: Product](
      baseDecoder: ConfDecoderEx[A],
      sectionName: String = null
  )(implicit presets: PartialFunction[Conf, A]): ConfDecoderEx[A] =
    (state, conf) =>
      decodePresets(conf, sectionName, presets) match {
        case Some(x: Configured.NotOk) => x
        case Some(Configured.Ok((obj, null))) => Configured.ok(obj)
        case Some(Configured.Ok((obj, cfg))) => baseDecoder.read(Some(obj), cfg)
        case _ => baseDecoder.read(state, conf)
      }

  private def decodePresets[A](
      conf: Conf,
      sectionName: String,
      presets: PartialFunction[Conf, A]
  ): Option[Configured[(A, Conf)]] = {
    def me = getClass.getSimpleName
    object presetsMatch {
      def unapply(conf: Conf): Option[A] = presets.lift(conf)
    }
    conf match {
      case Conf.Obj(v) => v.collectFirst { case (`presetKey`, x) => x }.map {
          case presetsMatch(x) =>
            val filtered = v.filter(_._1 != presetKey)
            val newConf = if (filtered.isEmpty) null else Conf.Obj(filtered)
            Configured.ok((x, newConf))
          case x => Configured.error(s"$me: unsupported preset: $x")
        }
      case presetsMatch(_) =>
        val section = Option(sectionName).fold("subsection '")(x => s"'$x.")
        val err = s"$me: top-level presets removed since v3.0.0; " +
          s"use $section$presetKey = $conf' instead"
        Some(Configured.error(err))
      case _ => None
    }
  }

}
