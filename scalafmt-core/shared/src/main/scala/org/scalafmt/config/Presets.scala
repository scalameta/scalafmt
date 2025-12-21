package org.scalafmt.config

import metaconfig._

object Presets {

  val presetKey = "preset"

  def contramap[A <: Product](
      baseDecoder: ConfDecoderEx[A],
  )(f: PartialFunction[Conf, Conf]): ConfDecoderEx[A] = baseDecoder.contramap {
    case conf @ Conf.Obj(v) => v.collectFirst { case (`presetKey`, x) => x }
        .flatMap(f.lift.apply)
        .fold(conf)(x => Conf.Obj((presetKey -> x) :: v.filter(_._1 != presetKey)))
    case conf => conf
  }

  def mapDecoder[A <: Product](
      baseDecoder: ConfDecoderEx[A],
      sectionName: String = null,
  )(implicit presets: PartialFunction[Conf, A]): ConfDecoderEx[A] =
    new ConfDecoderEx[A] {
      override def read(state: Option[A], conf: Conf): Configured[A] =
        decodePresets(conf, sectionName, presets) match {
          case Some(x: Configured.NotOk) => x
          case Some(Configured.Ok((obj, cfg))) =>
            if (cfg eq null) Configured.ok(obj)
            else baseDecoder.read(Some(obj), cfg)
          case _ => baseDecoder.read(state, conf)
        }
      override def convert(conf: Conf): Conf = baseDecoder.convert(conf)
    }

  def contramapDecoder[A <: Product](
      f: PartialFunction[Conf, Conf],
  )(baseDecoder: ConfDecoderEx[A], sectionName: String = null)(implicit
      presets: PartialFunction[Conf, A],
  ): ConfDecoderEx[A] = contramap(mapDecoder(baseDecoder, sectionName))(f)

  private def decodePresets[A](
      conf: Conf,
      sectionName: String,
      presets: PartialFunction[Conf, A],
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
