package org.scalafmt.config

import scala.collection.mutable

import metaconfig._

case class FileHeader(
    raw: Option[String] = None,
    text: Option[String] = None,
    license: Option[License] = None,
    licenseStyle: FileHeader.LicenseStyle = FileHeader.LicenseStyle.spdx,
    copyrightHolder: Option[String] = None,
    since: Option[Int] = None,
    year: Option[Int] = None,
    style: FileHeader.Style = FileHeader.Style.block,
    comment: FileHeader.Comment = FileHeader.Comment(),
    blankLineAfter: Boolean = true,
) {
  def isActive: Boolean = raw.isDefined || text.isDefined || license.isDefined

  // Whether this config would produce a docstring-style comment that the
  // formatter would try to reformat. Used by FormatWriter to bypass
  // docstring handling for the file header.
  def producesDocstringComment: Boolean =
    if (!isActive) false
    else raw match {
      case Some(r) => r.stripMargin.trim.startsWith("/**")
      case None =>
        val hasContent = text.exists(_.stripMargin.trim.nonEmpty) ||
          license.isDefined
        hasContent && (style eq FileHeader.Style.framed)
    }
}

object FileHeader {
  val default: FileHeader = FileHeader()

  implicit lazy val surface: generic.Surface[FileHeader] = generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[FileHeader] = generic.deriveEncoder

  private val baseDecoder: ConfDecoderEx[FileHeader] =
    generic.deriveDecoderEx(default).noTypos

  implicit lazy val decoder: ConfDecoderEx[FileHeader] =
    new ConfDecoderEx[FileHeader] {
      override def read(
          state: Option[FileHeader],
          conf: Conf,
      ): Configured[FileHeader] = conf match {
        case Conf.Str("none") | Conf.Bool(false) => Configured.ok(default)
        case Conf.Str(value) =>
          Configured.ok(default.copy(text = Some(value)))
        case _ => baseDecoder.read(state, conf).andThen(validate)
      }
    }

  private def validate(cfg: FileHeader): Configured[FileHeader] = {
    val errors = new mutable.ArrayBuffer[String]
    cfg.since.foreach { y =>
      if (y < 1900 || y > 2200) errors += s"fileHeader.since=$y is out of range"
    }
    cfg.year.foreach { y =>
      if (y < 1900 || y > 2200) errors += s"fileHeader.year=$y is out of range"
    }
    for { s <- cfg.since; e <- cfg.year }
      if (s > e) errors += "fileHeader.since must be <= fileHeader.year"
    cfg.comment.width.foreach { w =>
      if (w < 20) errors += "fileHeader.comment.width must be >= 20"
    }
    if (errors.isEmpty) Configured.ok(cfg)
    else Configured.error(errors.mkString("; "))
  }

  sealed abstract class LicenseStyle
  object LicenseStyle {
    case object spdx extends LicenseStyle
    case object detailed extends LicenseStyle
    implicit val codec: ConfCodecEx[LicenseStyle] =
      ConfCodecEx.oneOf[LicenseStyle](spdx, detailed)
  }

  sealed abstract class Style
  object Style {
    case object block extends Style
    case object line extends Style
    case object framed extends Style
    implicit val codec: ConfCodecEx[Style] =
      ConfCodecEx.oneOf[Style](block, line, framed)
  }

  case class Comment(
      style: Option[Docstrings.Style] = None,
      blankFirstLine: Option[Docstrings.BlankFirstLine] = None,
      blankLastLine: Boolean = false,
      width: Option[Int] = None,
  )

  object Comment {
    val default: Comment = Comment()
    implicit lazy val surface: generic.Surface[Comment] = generic.deriveSurface
    implicit lazy val encoder: ConfEncoder[Comment] = generic.deriveEncoder
    implicit lazy val decoder: ConfDecoderEx[Comment] = generic
      .deriveDecoderEx(default).noTypos
  }
}
