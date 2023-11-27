package org.scalafmt.config

import java.nio.file

import scala.annotation.tailrec
import scala.meta.Dialect
import scala.meta.dialects

import org.scalafmt.CompatCollections.JavaConverters._
import org.scalafmt.sysops.AbsoluteFile
import org.scalafmt.sysops.OsSpecific._

import metaconfig._
import metaconfig.annotation.DeprecatedName

case class ProjectFiles(
    git: Boolean = false,
    layout: Option[ProjectFiles.Layout] = None,
    includePaths: Seq[String] = ProjectFiles.defaultIncludePaths,
    excludePaths: Seq[String] = Nil,
    @DeprecatedName(
      "includeFilters",
      "use `includePaths` with `regex:` prefix",
      "v3.0.0"
    )
    includeFilters: Seq[String] = Nil,
    @DeprecatedName(
      "excludeFilters",
      "use `excludePaths` with `regex:` prefix",
      "v3.0.0"
    )
    excludeFilters: Seq[String] = Nil
) {
  // required for ScalafmtDynamic (ScalafmtReflectConfig.isIncludedInProject)
  lazy val matcher: ProjectFiles.FileMatcher = ProjectFiles.FileMatcher(this)
}

object ProjectFiles {
  implicit lazy val surface: generic.Surface[ProjectFiles] =
    generic.deriveSurface
  implicit lazy val codec: ConfCodecEx[ProjectFiles] =
    generic.deriveCodecEx(ProjectFiles()).noTypos

  private implicit val fs: file.FileSystem = file.FileSystems.getDefault

  val defaultIncludePaths =
    Seq("glob:**.scala", "glob:**.sbt", "glob:**.sc")

  private sealed abstract class PathMatcher {
    def matches(path: file.Path): Boolean
  }

  object FileMatcher {
    def apply(
        pf: ProjectFiles,
        regexExclude: Seq[String] = Nil
    ): FileMatcher = {
      // check if includePaths were specified explicitly
      val useIncludePaths =
        pf.includePaths.ne(defaultIncludePaths) || pf.includeFilters.isEmpty
      val includePaths = if (useIncludePaths) pf.includePaths else Seq.empty
      new FileMatcher(
        nio(includePaths) ++ regex(pf.includeFilters),
        nio(pf.excludePaths) ++ regex(pf.excludeFilters ++ regexExclude)
      )
    }

    private def create(seq: Seq[String], f: String => PathMatcher) =
      seq.map(_.asFilename).distinct.map(f)
    private def nio(seq: Seq[String]) = create(seq, new Nio(_))
    private def regex(seq: Seq[String]) = create(seq, new Regex(_))

    private final class Nio(pattern: String) extends PathMatcher {
      private val matcher = fs.getPathMatcher(pattern)
      def matches(path: file.Path): Boolean = matcher.matches(path)
    }
    private final class Regex(regex: String) extends PathMatcher {
      private val pattern = java.util.regex.Pattern.compile(regex)
      def matches(path: file.Path): Boolean =
        pattern.matcher(path.toString).find()
    }
  }

  class FileMatcher(include: Seq[PathMatcher], exclude: Seq[PathMatcher]) {
    def matchesPath(path: file.Path): Boolean =
      include.exists(_.matches(path)) && !exclude.exists(_.matches(path))
    def matches(filename: String): Boolean =
      matchesPath(fs.getPath(filename))
    def matchesFile(file: AbsoluteFile): Boolean =
      matchesPath(file.path)
  }

  sealed abstract class Layout {
    def getLang(path: AbsoluteFile): Option[String]
    protected[config] def getDialectByLang(lang: String)(implicit
        dialect: Dialect
    ): Option[NamedDialect]
    final def withLang(lang: String, style: ScalafmtConfig): ScalafmtConfig =
      style.withDialect(getDialectByLang(lang)(style.dialect))
  }

  object Layout {

    case object StandardConvention extends Layout {
      private val phaseLabels = Seq("main", "test", "it")

      override def getLang(path: AbsoluteFile): Option[String] = {
        val dirsIter = path.path.getParent.iterator().asScala
        val dirs = dirsIter.map(_.toString).toArray
        getLang(dirs, dirs.length)
      }

      @tailrec
      private def getLang(dirs: Array[String], len: Int): Option[String] = {
        val srcIdx = dirs.lastIndexOf("src", len - 3)
        if (srcIdx < 0) None
        else {
          val langIdx = srcIdx + 2
          val found = phaseLabels.contains(dirs(srcIdx + 1))
          if (found) Some(dirs(langIdx)) else getLang(dirs, langIdx)
        }
      }

      @inline private def is211(implicit dialect: Dialect) =
        !dialect.allowCaseClassWithoutParameterList
      @inline private def is212(implicit dialect: Dialect) =
        dialect.allowTrailingCommas
      @inline private def is213(implicit dialect: Dialect) =
        dialect.allowLiteralTypes
      @inline private def is3(implicit dialect: Dialect) =
        dialect.allowSignificantIndentation

      @inline private[config] def nd(text: sourcecode.Text[Dialect]) =
        Some(NamedDialect(text))
      private[config] val s210 = nd(dialects.Scala210)
      private[config] val s211 = nd(dialects.Scala211)
      private[config] val s212 = nd(NamedDialect.scala212)
      private[config] val s213 = nd(NamedDialect.scala213)
      private[config] val s3 = nd(NamedDialect.scala3)

      protected[config] override def getDialectByLang(lang: String)(implicit
          dialect: Dialect
      ): Option[NamedDialect] = lang match {
        case "scala-2.10" if is211 => s210
        case "scala-2.11" if is212 || !is211 => s211
        case "scala-2.12" if is213 || !is212 => s212
        case "scala-2.13" if is3 || !is213 => s213
        case "scala-2" if is3 => s213
        case "scala-3" if !is3 => s3
        case _ => None
      }
    }

    implicit val reader: ConfCodecEx[Layout] =
      ReaderUtil.oneOf[Layout](
        StandardConvention
      )

  }

}
