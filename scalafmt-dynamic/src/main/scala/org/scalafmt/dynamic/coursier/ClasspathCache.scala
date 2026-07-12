package org.scalafmt.dynamic.coursier

import org.scalafmt.CompatCollections.JavaConverters._

import java.io.File
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardCopyOption}
import java.security.MessageDigest

import scala.util.Try

import _root_.coursier._

/** Disk cache of resolved classpaths (scalafmt version -> jar files), so a
  * fresh `scalafmt` process skips coursier resolution when it has run that
  * version before. Keyed by the version plus scala version and repositories; a
  * hit is only honored if every cached jar still exists (the coursier cache may
  * have been cleaned).
  *
  * Only immutable versions are cached: SNAPSHOT/nightly (`+`) versions resolve
  * to changing artifacts and are always re-fetched. Any failure (unwritable
  * cache dir, corrupt entry) degrades silently to a normal fetch.
  */
private[coursier] object ClasspathCache {

  private def cacheDir: Option[Path] = Try {
    val base = paths.CoursierPaths.cacheDirectory().getParentFile
    val dir = new File(base, "scalafmt/classpath").toPath
    Files.createDirectories(dir)
    dir
  }.toOption

  private def cacheable(version: String): Boolean =
    !version.contains("SNAPSHOT") && !version.contains("+")

  private def key(
      v: String,
      scalaV: String,
      repos: Seq[MavenRepository],
  ): String = {
    val md = MessageDigest.getInstance("SHA-1")
    md.update(repos.mkString("\n").getBytes(UTF_8))
    val repoHash = md.digest().take(6).map("%02x".format(_)).mkString
    s"$v-scala$scalaV-$repoHash.classpath"
  }

  def get(
      v: String,
      scalaV: String,
      repos: Seq[MavenRepository],
  ): Option[Seq[File]] =
    if (!cacheable(v)) None
    else cacheDir.flatMap(dir =>
      Try {
        val files = Files.readAllLines(dir.resolve(key(v, scalaV, repos)), UTF_8)
          .asScala.map(new File(_)).toSeq
        // honor only if non-empty and every jar still present
        if (files.nonEmpty && files.forall(_.isFile)) Some(files) else None
      }.toOption.flatten,
    )

  def put(
      v: String,
      scalaV: String,
      repos: Seq[MavenRepository],
      files: Seq[File],
  ): Unit = if (cacheable(v) && files.nonEmpty) cacheDir.foreach(dir =>
    Try {
      val content = files.map(_.getAbsolutePath).mkString("\n").getBytes(UTF_8)
      val tmp = Files.createTempFile(dir, "cp", ".tmp")
      Files.write(tmp, content)
      // atomic rename so concurrent readers never see a partial file
      Files.move(
        tmp,
        dir.resolve(key(v, scalaV, repos)),
        StandardCopyOption.REPLACE_EXISTING,
        StandardCopyOption.ATOMIC_MOVE,
      )
    },
  )

}
