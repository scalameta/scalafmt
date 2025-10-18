package org.scalafmt.sysops

import java.io.{ByteArrayOutputStream, InputStream}
import java.nio.file.{Path, Paths}

import scala.util.{Failure, Success, Try}

object FileOps {

  val defaultConfigFileName = ".scalafmt.conf"

  def listFiles(path: String): Seq[Path] = listFiles(getPath(path))

  def listFiles(file: Path): Seq[Path] =
    listFiles((_, a) => a.isRegularFile)(file)

  def listFiles(matches: (Path, FileStat) => Boolean)(file: Path): Seq[Path] =
    PlatformFileOps.listFiles(file, matches)

  def walkFiles(visitor: WalkVisitor)(file: Path): Seq[Path] = PlatformFileOps
    .walkFiles(visitor)(file)

  @inline
  def getFile(path: Seq[String]): Path = getPath(path.head, path.tail: _*)

  @inline
  def getPath(head: String, tail: String*): Path = Paths.get(head, tail: _*)

  @inline
  def isMarkdown(filename: String): Boolean = filename.endsWith(".md")

  @inline
  def isAmmonite(filename: String): Boolean = filename.endsWith(".sc")

  @inline
  def isSbt(filename: String): Boolean = filename.endsWith(".sbt")

  def getCanonicalConfigFile(
      workingDirectory: AbsoluteFile,
      config: Option[Path] = None,
  ): Option[Try[Path]] = config.fold(tryGetConfigInDir(workingDirectory)) { x =>
    val file = workingDirectory.join(x)
    tryCheckConfigFile(file)
      .orElse(Some(Failure(new RuntimeException(s"Config missing: $file"))))
  }

  def tryGetConfigInDir(dir: AbsoluteFile): Option[Try[Path]] =
    tryCheckConfigFile(dir / defaultConfigFileName)

  private def tryCheckConfigFile(file: AbsoluteFile): Option[Try[Path]] =
    if (!file.exists) None
    else if (file.isRegularFile) Some(Success(file.path))
    else Some(Failure(new RuntimeException(s"Config not a file: $file")))

  def readInputStream(is: InputStream): String = {
    val baos = new ByteArrayOutputStream()
    val buf = new Array[Byte](1024)
    var cnt = 0
    while ({ cnt = is.read(buf); cnt >= 0 }) baos.write(buf, 0, cnt)
    baos.toString("utf-8")
  }

  sealed trait WalkVisit
  object WalkVisit {
    case object Good extends WalkVisit
    case object Stop extends WalkVisit
    case object Skip extends WalkVisit
  }

  abstract class WalkVisitor {
    def onTree(dir: Path, fileStat: FileStat): WalkVisit = WalkVisit.Good
    def onFile(file: Path, fileStat: FileStat): WalkVisit = WalkVisit.Good
    def onFailStop(file: Path, exc: Throwable): Boolean = throw exc
  }
  object WalkVisitor {
    val empty: WalkVisitor = new WalkVisitor {}
  }

}
