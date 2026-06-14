package org.scalafmt.config

import org.scalafmt.config.RewriteScala3Settings._

import metaconfig._

@annotation.SectionRename("countEndMarkerLines", "endMarker.spanHas") // renamed in v3.10.3
@annotation.SectionRename("removeEndMarkerMaxLines", "endMarker.removeMaxSpan") // renamed in v3.10.3
@annotation.SectionRename("insertEndMarkerMinLines", "endMarker.insertMinSpan") // renamed in v3.10.3
@annotation.SectionRename("removeOptionalBraces", "optionalBraces") // renamed in v3.10.8
case class RewriteScala3Settings(
    convertToNewSyntax: Boolean = false,
    newSyntax: ConvertToNewSyntax = ConvertToNewSyntax.default,
    optionalBraces: RemoveOptionalBraces = RemoveOptionalBraces.no,
    endMarker: EndMarker = EndMarker.default,
)

object RewriteScala3Settings {
  implicit val surface: generic.Surface[RewriteScala3Settings] =
    generic.deriveSurface
  implicit val encoder: ConfEncoder[RewriteScala3Settings] = generic
    .deriveEncoder[RewriteScala3Settings]

  val default = new RewriteScala3Settings

  private val styleGuideCommon = new RewriteScala3Settings(
    convertToNewSyntax = true,
    newSyntax = ConvertToNewSyntax(deprecated = false),
    optionalBraces = RemoveOptionalBraces(insert =
      Some(BracesFilters(blankGaps = Between(min = 1))),
    ),
    endMarker = EndMarker(remove = EndMarker.Filters(blankGaps = Between(min = 1))),
  )

  implicit val decodec: ConfDecoderEx[RewriteScala3Settings] = Presets
    .mapDecoder(
      generic.deriveDecoderEx(default).noTypos.detectSectionRenames,
      "rewrite.scala3",
    ) {
      case Conf.Str("common") => styleGuideCommon
      case Conf.Bool(true) => new RewriteScala3Settings(
          convertToNewSyntax = true,
          optionalBraces = RemoveOptionalBraces.yes,
        )
      case Conf.Bool(false) => default
    }

  @annotation.SectionRename("fewerBracesMinSpan", "fewerBraces.minSpan") // 3.10.8
  @annotation.SectionRename("fewerBracesMaxSpan", "fewerBraces.maxSpan") // 3.10.8
  @annotation.SectionRename("fewerBracesParensToo", "fewerBraces.parensToo") // 3.10.8
  case class RemoveOptionalBraces(
      enabled: Boolean = true,
      preferInsert: Boolean = true,
      insert: Option[BracesFilters] = None,
      remove: Option[BracesFilters] = None,
      fewerBraces: FewerBraces = FewerBraces.default,
      oldSyntaxToo: Boolean = false,
  ) {
    def isRemoveEnabled: Boolean = remove.forall(_.enabled)
    def isInsertEnabled: Boolean = insert.exists(_.enabled)

    private[config] def normalized: RemoveOptionalBraces = {
      val ibOpt = insert.filter(_.enabled)
      val rbOpt = remove.filter(_.enabled)
      ibOpt.fold(copy(insert = None, remove = rbOpt))(ib =>
        rbOpt.fold( // if insert is Some, remove must be too
          copy(insert = ibOpt, remove = Some(BracesFilters.disabled)),
        )(rb =>
          if (preferInsert) copy(insert = ibOpt, remove = Some(rb.exclude(ib)))
          else copy(insert = Some(ib.exclude(rb)), remove = rbOpt),
        ),
      )
    }
  }

  object RemoveOptionalBraces {

    val yes = RemoveOptionalBraces()
    val no = RemoveOptionalBraces(enabled = false)

    implicit val surface: generic.Surface[RemoveOptionalBraces] =
      generic.deriveSurface

    implicit val encoder: ConfEncoder[RemoveOptionalBraces] = generic
      .deriveEncoder[RemoveOptionalBraces]

    implicit final val decoder: ConfDecoderEx[RemoveOptionalBraces] = generic
      .deriveDecoderEx[RemoveOptionalBraces](no).map(_.normalized).noTypos
      .detectSectionRenames.contramapPartial {
        case Conf.Bool(true) | Conf.Str("yes") => Conf
            .Obj("enabled" -> Conf(true))
        case Conf.Bool(false) | Conf.Str("no") => Conf
            .Obj("enabled" -> Conf(false))
        case Conf.Str("oldSyntaxToo") => Conf
            .Obj("enabled" -> Conf(true), "oldSyntaxToo" -> Conf(true))
      }

  }

  @annotation.SectionRename("minSpan", "span.min") // 3.11.2
  @annotation.SectionRename("maxSpan", "span.max") // 3.11.2
  @annotation.SectionRename("minBlankGaps", "blankGaps.min") // 3.11.2
  @annotation.SectionRename("maxBlankGaps", "blankGaps.max") // 3.11.2
  case class BracesFilters(
      span: Between = Between.disabled,
      blankGaps: Between = Between.disabled,
  ) {
    def enabled: Boolean = span.enabled || blankGaps.enabled

    def exclude(that: BracesFilters): BracesFilters = {
      val span = this.span.exclude(that.span)
      val blankGaps = this.blankGaps.exclude(that.blankGaps)
      if ((span eq this.span) && (blankGaps eq this.blankGaps)) this
      else copy(span = span, blankGaps = blankGaps)
    }
  }

  object BracesFilters {
    val disabled = new BracesFilters()
    implicit val surface: generic.Surface[BracesFilters] = generic.deriveSurface
    implicit val codec: ConfCodecEx[BracesFilters] = generic
      .deriveCodecEx(disabled).noTypos.detectSectionRenames
  }

  @annotation.SectionRename("minSpan", "span.min") // 3.11.2
  @annotation.SectionRename("maxSpan", "span.max") // 3.11.2
  case class FewerBraces(
      span: Between = Between(min = 2),
      parensToo: Boolean = false,
  )
  object FewerBraces {
    val default = new FewerBraces()
    implicit val surface: generic.Surface[FewerBraces] = generic.deriveSurface
    implicit val codec: ConfCodecEx[FewerBraces] = generic.deriveCodecEx(default)
      .noTypos.detectSectionRenames
  }

  case class EndMarker(
      spanHas: EndMarker.SpanHas = EndMarker.SpanHas.all,
      insert: EndMarker.Filters = EndMarker.Filters.disabled,
      remove: EndMarker.Filters = EndMarker.Filters.disabled,
      preferInsert: Boolean = true,
  )

  object EndMarker {

    val default = new EndMarker
    implicit val surface: generic.Surface[EndMarker] = generic.deriveSurface
    implicit val encoder: ConfEncoder[EndMarker] = generic
      .deriveEncoder[EndMarker]
    implicit val decoder: ConfDecoderEx[EndMarker] = generic
      .deriveDecoderEx(default).noTypos
      .contramapPartial { case conf: Conf.Obj =>
        var useBlankGaps = false
        conf.removeKeyIfVal("spanIs") {
          case Conf.Str("lines") => null
          case Conf.Str("blankGaps") => useBlankGaps = true; null
        }.fold(conf)(x => Conf.Obj(x._2)).replace {
          case ("insertMinSpan", v: Conf.Num) =>
            val kv =
              if (useBlankGaps) "minBlankGaps" -> v
              else "minBreaks" -> Conf.Num(v.value - 1)
            List((Conf.nameOf(default.insert), Conf.Obj(kv)))
          case ("removeMaxSpan", v: Conf.Num) =>
            val kv =
              if (useBlankGaps) "maxBlankGaps" -> v
              else "maxBreaks" -> Conf.Num(v.value - 1)
            List((Conf.nameOf(default.remove), Conf.Obj(kv)))
        }
      }

    sealed abstract class SpanHas
    object SpanHas {
      implicit val codec: ConfCodecEx[SpanHas] = ConfCodecEx
        .oneOf(all, lastBlockOnly)
      case object all extends SpanHas
      case object lastBlockOnly extends SpanHas
    }

    @annotation.SectionRename("minBreaks", "breaks.min") // 3.11.2
    @annotation.SectionRename("maxBreaks", "breaks.max") // 3.11.2
    @annotation.SectionRename("minBlankGaps", "blankGaps.min") // 3.11.2
    @annotation.SectionRename("maxBlankGaps", "blankGaps.max") // 3.11.2
    case class Filters(
        breaks: Between = Between.disabled,
        blankGaps: Between = Between.disabled,
    ) {
      def enabled: Boolean = breaks.enabled || blankGaps.enabled
    }
    object Filters {
      val disabled = new Filters()
      implicit val surface: generic.Surface[Filters] = generic.deriveSurface
      implicit val codec: ConfCodecEx[Filters] = generic.deriveCodecEx(disabled)
        .noTypos.detectSectionRenames
    }
  }

  case class Between(min: Int = -1, max: Int = -1) {
    def enabled: Boolean = if (max >= 0) max >= min else min >= 0
    def satisfied(fvalue: => Int): Boolean =
      min <= 0 && (max < 0 || max == Int.MaxValue) || {
        val value = fvalue
        min <= value && (max < 0 || value <= max)
      }
    def overlaps(other: Between): Boolean =
      if (min < 0)
        if (other.min < 0) max >= 0 && other.max >= 0 else max >= other.min
      else if (max < 0) if (other.max < 0) other.min >= 0 else min <= other.max
      else if (other.min < 0) min <= other.max
      else if (other.max < 0) max >= other.min
      else max >= other.min && min <= other.max
    def exclude(other: Between): Between =
      if (other.enabled) copy(
        min = if (other.max < 0) min else min.max(other.max + 1),
        max = if (other.min < 0) max else max.min(other.min - 1),
      )
      else this
  }
  object Between {
    val disabled = new Between()
    implicit val surface: generic.Surface[Between] = generic.deriveSurface
    implicit val codec: ConfCodecEx[Between] = generic.deriveCodecEx(disabled)
      .noTypos
  }

  case class ConvertToNewSyntax(
      // https://dotty.epfl.ch/docs/reference/other-new-features/control-syntax.html
      control: Boolean = true,
      // https://dotty.epfl.ch/docs/reference/changed-features/vararg-splices.html
      // https://dotty.epfl.ch/docs/reference/changed-features/imports.html
      // https://dotty.epfl.ch/docs/reference/changed-features/wildcards.html
      deprecated: Boolean = true,
  )

  private object ConvertToNewSyntax {

    val default = new ConvertToNewSyntax

    implicit val surface: generic.Surface[ConvertToNewSyntax] =
      generic.deriveSurface
    implicit val codec: ConfCodecEx[ConvertToNewSyntax] = generic
      .deriveCodecEx(default).noTypos

  }

}
