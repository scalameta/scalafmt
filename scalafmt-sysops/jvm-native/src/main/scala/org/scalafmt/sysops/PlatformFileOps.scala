package org.scalafmt.sysops

import org.scalafmt.CompatCollections.JavaConverters._

import java.io.IOException
import java.nio.file._

import scala.concurrent.Future
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

  def tryGetFileStat(file: Path, followLinks: Boolean): Try[FileStat] = {
    val linkOpts = if (followLinks) Nil else Seq(LinkOption.NOFOLLOW_LINKS)
    Try(Files.readAttributes(
      file,
      classOf[attribute.BasicFileAttributes],
      linkOpts: _*,
    )).map(new NioFileStat(_))
  }

  def getFileStat(file: Path, followLinks: Boolean): Option[FileStat] =
    tryGetFileStat(file, followLinks).toOption

  def listFiles(file: Path, matches: (Path, FileStat) => Boolean): Seq[Path] = {
    val iter = Files
      .find(file, Integer.MAX_VALUE, (p, a) => matches(p, new NioFileStat(a)))
    try iter.iterator().asScala.toList
    finally iter.close()
  }

  def walkFiles(visitor: FileOps.WalkVisitor)(basePath: Path): Seq[Path] = {
    val res = Seq.newBuilder[Path]
    val simpleFileVisitor = new SimpleFileVisitor[Path] {
      override def preVisitDirectory(
          dir: Path,
          attrs: attribute.BasicFileAttributes,
      ): FileVisitResult = visitor.onTree(dir, new NioFileStat(attrs)) match {
        case FileOps.WalkVisit.Stop => FileVisitResult.TERMINATE
        case FileOps.WalkVisit.Skip => FileVisitResult.SKIP_SUBTREE
        case FileOps.WalkVisit.Good => FileVisitResult.CONTINUE
      }
      override def visitFile(
          file: Path,
          attrs: attribute.BasicFileAttributes,
      ): FileVisitResult = visitor.onFile(file, new NioFileStat(attrs)) match {
        case FileOps.WalkVisit.Stop => FileVisitResult.TERMINATE
        case FileOps.WalkVisit.Skip => FileVisitResult.CONTINUE
        case FileOps.WalkVisit.Good => res += file; FileVisitResult.CONTINUE
      }
      override def visitFileFailed(
          file: Path,
          exc: IOException,
      ): FileVisitResult =
        if (visitor.onFailStop(file, exc)) FileVisitResult.TERMINATE
        else FileVisitResult.CONTINUE
    }

    Files.walkFileTree(basePath, simpleFileVisitor)
    res.result()
  }

  def readFile(file: Path)(implicit codec: Codec): String =
    new String(Files.readAllBytes(file), codec.charSet)

  def readFileAsync(file: Path)(implicit codec: Codec): Future[String] =
    GranularPlatformAsyncOps.readFileAsync(file)

  def readStdinAsync: Future[String] = Future
    .successful(FileOps.readInputStream(System.in))

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = {
    val bytes = content.getBytes(codec.charSet)
    Files.write(path, bytes)
  }

  def writeFileAsync(path: Path, content: String)(implicit
      codec: Codec,
  ): Future[Unit] = GranularPlatformAsyncOps.writeFileAsync(path, content)

  def cwd() = System.getProperty("user.dir")
}
