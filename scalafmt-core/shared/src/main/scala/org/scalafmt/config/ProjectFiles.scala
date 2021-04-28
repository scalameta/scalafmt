package org.scalafmt.config

import java.nio.file

import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.OsSpecific._

import metaconfig._
import metaconfig.annotation.DeprecatedName

case class ProjectFiles(
    git: Boolean = false,
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
  val reader: ConfDecoder[ProjectFiles] = generic.deriveDecoder(this).noTypos

  // required for ScalafmtDynamic (ScalafmtReflectConfig.isIncludedInProject)
  lazy val matcher: ProjectFiles.FileMatcher = ProjectFiles.FileMatcher(this)
}

object ProjectFiles {
  implicit lazy val surface: generic.Surface[ProjectFiles] =
    generic.deriveSurface
  implicit lazy val encoder: ConfEncoder[ProjectFiles] =
    generic.deriveEncoder

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
      matchesPath(file.jfile.toPath)
  }

}
