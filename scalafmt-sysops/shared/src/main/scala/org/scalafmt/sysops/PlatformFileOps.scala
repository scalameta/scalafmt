package org.scalafmt.sysops

import org.scalafmt.CompatCollections.JavaConverters._

import java.nio.file._

import scala.io.Codec
import scala.util.Try

object PlatformFileOps {

  private class NioFileStat(obj: attribute.BasicFileAttributes) extends FileStat {
    def isDirectory: Boolean = obj.isDirectory
    def isRegularFile: Boolean = obj.isRegularFile
    def isSymlink: Boolean = obj.isSymbolicLink
  }

  def exists(file: Path): Boolean = Files.exists(file)

  def symlink(link: Path, file: Path): Unit = Files
    .createSymbolicLink(link, file)

  def mkdir(file: Path): Unit = Files.createDirectory(file)
  def mkdirs(file: Path): Unit = Files.createDirectories(file)
  def mkdtemp(prefix: String): Path = Files.createTempDirectory(prefix)

  def move(src: Path, dst: Path): Unit = Files
    .move(src, dst, java.nio.file.StandardCopyOption.REPLACE_EXISTING)

  def delete(file: Path): Unit = Files.delete(file)

  def isDirectory(file: Path): Boolean = Files.isDirectory(file)
  def isRegularFile(file: Path): Boolean = Files.isRegularFile(file)
  def isRegularFileNoLinks(file: Path): Boolean = Files
    .isRegularFile(file, LinkOption.NOFOLLOW_LINKS)

  def getFileStat(file: Path, followLinks: Boolean): Option[FileStat] = {
    val linkOpts = if (followLinks) Nil else Seq(LinkOption.NOFOLLOW_LINKS)
    Try(Files.readAttributes(
      file,
      classOf[attribute.BasicFileAttributes],
      linkOpts: _*,
    )).toOption.map(new NioFileStat(_))
  }

  def listFiles(file: Path, matches: (Path, FileStat) => Boolean): Seq[Path] = {
    val iter = Files
      .find(file, Integer.MAX_VALUE, (p, a) => matches(p, new NioFileStat(a)))
    try iter.iterator().asScala.toList
    finally iter.close()
  }

  def readFile(file: Path)(implicit codec: Codec): String =
    new String(Files.readAllBytes(file), codec.charSet)

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

  def cwd() = System.getProperty("user.dir")
}
