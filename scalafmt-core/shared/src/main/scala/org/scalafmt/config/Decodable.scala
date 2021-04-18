package org.scalafmt.config

import metaconfig._

object Decodable {

  val presetKey = "preset"

}

abstract class Decodable[A](sectionName: String = null) { self: A =>
  type T = A
  protected[config] def baseDecoder: ConfDecoder[A]

  implicit final def decoder(implicit
      presets: PartialFunction[Conf, A]
  ): ConfDecoder[A] =
    new ConfDecoder[A] {
      override def read(conf: Conf): Configured[A] = {
        decodePresets(conf, presets) match {
          case Some(x: Configured.NotOk) => x
          case Some(Configured.Ok((obj, null))) => Configured.ok(obj)
          case Some(Configured.Ok((obj, cfg))) =>
            obj.asInstanceOf[Decodable[A]].baseDecoder.read(cfg)
          case _ => self.baseDecoder.read(conf)
        }
      }
    }

  protected def decodePresets(
      conf: Conf,
      presets: PartialFunction[Conf, A]
  ): Option[Configured[(A, Conf)]] = {
    def me = getClass.getSimpleName
    object presetsMatch {
      def unapply(conf: Conf): Option[A] = presets.lift(conf)
    }
    conf match {
      case Conf.Obj(v) =>
        v.collectFirst { case (Decodable.presetKey, x) => x }.map {
          case presetsMatch(x) =>
            val filtered = v.filter(_._1 != Decodable.presetKey)
            val newConf = if (filtered.isEmpty) null else Conf.Obj(filtered)
            Configured.ok((x, newConf))
          case x =>
            ConfError.message(s"$me: unsupported preset: $x").notOk
        }
      case presetsMatch(_) =>
        val section = Option(sectionName).fold("subsection '")(x => s"'$x.")
        val err = s"$me: top-level presets removed since v3.0.0; " +
          s"use $section${Decodable.presetKey} = $conf' instead"
        Some(ConfError.message(err).notOk)
      case _ => None
    }
  }

}
