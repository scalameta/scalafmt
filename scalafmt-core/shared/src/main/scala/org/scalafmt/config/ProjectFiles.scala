package org.scalafmt.config

import org.scalafmt.sysops._

import scala.meta.Dialect
import scala.meta.dialects

import java.nio.file

import scala.annotation.tailrec

import metaconfig._

case class ProjectFiles(
    git: Boolean = false,
    layout: Option[ProjectFiles.Layout] = None,
    includePaths: Seq[String] = ProjectFiles.defaultIncludePaths,
    excludePaths: Seq[String] = Nil,
    @annotation.DeprecatedName(
      "includeFilters",
      "use `includePaths` with `regex:` prefix",
      "v3.0.0",
    )
    includeFilters: Seq[String] = Nil,
    @annotation.DeprecatedName(
      "excludeFilters",
      "use `excludePaths` with `regex:` prefix",
      "v3.0.0",
    )
    excludeFilters: Seq[String] = Nil,
) {
  // required for ScalafmtDynamic (ScalafmtReflectConfig.isIncludedInProject)
  lazy val matcher: ProjectFiles.FileMatcher = ProjectFiles.FileMatcher(this)
}

object ProjectFiles {
  implicit lazy val surface: generic.Surface[ProjectFiles] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[ProjectFiles] = generic
    .deriveCodecEx(ProjectFiles()).noTypos

  val defaultIncludePaths =
    Seq("glob:**.scala", "glob:**.sbt", "glob:**.sc", "glob:**.mill")

  object FileMatcher {
    def apply(
        pf: ProjectFiles,
        regexExclude: Seq[String] = Nil,
    ): FileMatcher = {
      // check if includePaths were specified explicitly
      val useIncludePaths = pf.includePaths.ne(defaultIncludePaths) ||
        pf.includeFilters.isEmpty
      val includePaths = if (useIncludePaths) pf.includePaths else Seq.empty
      new FileMatcher(
        nio(includePaths) ++ regex(pf.includeFilters),
        nio(pf.excludePaths) ++ regex(pf.excludeFilters ++ regexExclude),
      )
    }

    private def create(seq: Seq[String], f: String => PathMatcher) = seq
      .map(OsSpecific.inPathMatcherForm).distinct.map(f)
    private def nio(seq: Seq[String]) = create(seq, PlatformPathMatcher.apply)
    private def regex(seq: Seq[String]) = create(seq, PathMatcher.Regex.apply)

  }

  class FileMatcher(include: Seq[PathMatcher], exclude: Seq[PathMatcher]) {
    def matchesPath(path: file.Path): Boolean = include
      .exists(_.matches(path)) && !exclude.exists(_.matches(path))
    def matches(filename: String): Boolean =
      matchesPath(FileOps.getPath(filename))
    def matchesFile(file: AbsoluteFile): Boolean = matchesPath(file.path)
  }

  case class FileInfo(lang: String, isTest: Boolean)

  sealed abstract class Layout {
    def getInfo(path: AbsoluteFile): Option[FileInfo]
    protected[config] def getDialectByLang(lang: String)(implicit
        dialect: Dialect,
    ): Option[NamedDialect]
    final def getLang(path: AbsoluteFile): Option[String] = getInfo(path)
      .map(_.lang)
    final def withLang(lang: String, style: ScalafmtConfig): ScalafmtConfig =
      style.withDialect(getDialectByLang(lang)(style.dialect))
  }

  object Layout {

    case object StandardConvention extends Layout {
      private val mainLabels = Seq("main")
      private val testLabels = Seq("test", "it")

      override def getInfo(af: AbsoluteFile): Option[FileInfo] = {
        val parent = af.path.getParent
        val depth = parent.getNameCount
        val dirs = new Array[String](depth)
        for (i <- 0 until depth) dirs(i) = parent.getName(i).toString
        getInfo(dirs, depth)
      }

      @tailrec
      private def getInfo(dirs: Array[String], len: Int): Option[FileInfo] = {
        // src/phase/lang
        val srcIdx = dirs.lastIndexOf("src", len - 3)
        if (srcIdx < 0) None
        else {
          val phase = dirs(srcIdx + 1)
          def lang = dirs(srcIdx + 2)
          if (mainLabels.contains(phase)) Some(FileInfo(lang, false))
          else if (testLabels.contains(phase)) Some(FileInfo(lang, true))
          else getInfo(dirs, srcIdx)
        }
      }

      @inline
      private def is211(implicit dialect: Dialect) =
        !dialect.allowCaseClassWithoutParameterList
      @inline
      private def is212(implicit dialect: Dialect) = dialect.allowTrailingCommas
      @inline
      private def is213(implicit dialect: Dialect) = dialect.allowLiteralTypes
      @inline
      private def is3(implicit dialect: Dialect) =
        dialect.allowSignificantIndentation

      @inline
      private[config] def nd(text: sourcecode.Text[Dialect]) =
        Some(NamedDialect(text))
      private[config] val s210 = nd(dialects.Scala210)
      private[config] val s211 = nd(dialects.Scala211)
      private[config] val s212 = nd(dialects.Scala212)
      private[config] val s213 = nd(dialects.Scala213)
      private[config] val s3 = nd(dialects.Scala3)

      override protected[config] def getDialectByLang(
          lang: String,
      )(implicit dialect: Dialect): Option[NamedDialect] = lang match {
        case "scala-2.10" if is211 => s210
        case "scala-2.11" if is212 || !is211 => s211
        case "scala-2.12" if is213 || !is212 => s212
        case "scala-2.13" if is3 || !is213 => s213
        case "scala-2" if is3 => s213
        case "scala-3" if !is3 => s3
        case _ => None
      }
    }

    implicit val reader: ConfCodecEx[Layout] = ReaderUtil
      .oneOf[Layout](StandardConvention)

  }

}
