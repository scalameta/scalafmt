package org.scalafmt.rewrite

import org.scalafmt.config.ReaderUtil
import org.scalafmt.config.RewriteSettings

import metaconfig._

object Imports {

  case class Settings(
      sort: Sort = Sort.none,
      expand: Boolean = false,
      groups: Seq[Seq[String]] = Nil
  ) {
    def nonEmpty: Boolean = sort.ne(Sort.none) || expand || groups.nonEmpty
  }

  object Settings {
    implicit val surface: generic.Surface[Settings] = generic.deriveSurface
    implicit val codec: ConfCodecEx[Settings] =
      generic.deriveCodecEx(new Settings).noTypos
  }

  private val allImportRules: Set[Rewrite] =
    Set(ExpandImportSelectors, SortImports, AsciiSortImports)

  def validateImports(obj: RewriteSettings): Configured[RewriteSettings] = {
    val importRules = obj.rules.filter(allImportRules.contains)
    if (importRules.lengthCompare(1) > 0) {
      val msg = importRules.mkString("Incompatible rewrites: ", ", ", "")
      Configured.NotOk(ConfError.message(msg))
    } else Configured.Ok(obj)
  }

  sealed abstract class Sort {
    // TODO
  }

  private object Sort {

    implicit val reader: ConfCodecEx[Sort] =
      ReaderUtil.oneOf[Sort](none, ascii, original, scalastyle)

    case object none extends Sort {
      // TODO
    }

    abstract class SortBase extends Sort {
      // TODO
    }

    case object ascii extends SortBase {
      // TODO
    }

    case object original extends SortBase {
      // TODO
    }

    case object scalastyle extends SortBase {
      // TODO
    }
  }

}
