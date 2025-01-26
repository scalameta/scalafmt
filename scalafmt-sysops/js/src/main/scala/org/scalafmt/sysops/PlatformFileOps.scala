package org.scalafmt.sysops

import scala.meta.internal.io._

import java.nio.file._

import scala.concurrent.Future
import scala.io.Codec
import scala.scalajs.js
import scala.util.Try

object PlatformFileOps {

  private class JSFileStat(obj: JSStats) extends FileStat {
    def isDirectory: Boolean = obj.isDirectory()
    def isRegularFile: Boolean = obj.isFile()
    def isSymlink: Boolean = obj.isSymbolicLink()
  }

  def exists(path: String): Boolean = JSFs.existsSync(path)
  def exists(file: Path): Boolean = exists(file.toString)

  def symlink(link: Path, file: Path): Unit = JSFs
    .symlinkSync(file.toString, link.toString)

  def mkdir(dir: String, recursive: Boolean): Unit = JSFs
    .mkdirSync(dir, js.Dynamic.literal(recursive = recursive))
  def mkdir(file: Path): Unit = mkdir(file.toString, recursive = false)
  def mkdirs(file: Path): Unit = mkdir(file.toString, recursive = true)
  def mkdtemp(prefix: String): Path = Paths
    .get(JSFs.mkdtempSync(Paths.get(JSOS.tmpdir()).resolve(prefix).toString))

  def move(src: Path, dst: Path): Unit = JSFs
    .renameSync(src.toString, dst.toString)

  def delete(path: String): Unit = JSFs.unlinkSync(path)
  def delete(file: Path): Unit = delete(file.toString)
  def rmdir(path: String, recursive: Boolean, force: Boolean): Unit = JSFs
    .rmSync(path, js.Dynamic.literal(recursive = recursive, force = force))

  def isDirectory(file: Path): Boolean = getFileStat(file, followLinks = false)
    .exists(_.isDirectory)
  def isRegularFile(file: Path): Boolean = getFileStat(file, followLinks = true)
    .exists(_.isRegularFile)
  def isRegularFileNoLinks(file: Path): Boolean =
    getFileStat(file, followLinks = false).exists(_.isRegularFile)

  def getFileStat(file: String, followLinks: Boolean): Option[FileStat] =
    Try(if (followLinks) JSFs.statSync(file) else JSFs.lstatSync(file)).toOption
      .map(new JSFileStat(_))

  def getFileStat(file: Path, followLinks: Boolean): Option[FileStat] =
    getFileStat(file.toString, followLinks)

  def listFiles(
      basePath: Path,
      matches: (Path, FileStat) => Boolean,
  ): Seq[Path] = {
    import scala.collection.mutable.ListBuffer
    val res = Seq.newBuilder[Path]
    val paths = new ListBuffer[Path]()
    paths.append(basePath)
    while (paths.nonEmpty) {
      val path = paths.remove(paths.length - 1)
      getFileStat(path.toString, followLinks = false).foreach(fileStat =>
        if (fileStat.isDirectory) JSFs.readdirSync(path.toString)
          .foreach(entry => paths.append(path.resolve(entry)))
        else if (matches(path, fileStat)) res += path,
      )
    }
    res.result()
  }

  def readFile(path: Path)(implicit codec: Codec): String = JSFs
    .readFileSync(path.toString, codec.name)

  def readFileAsync(file: Path)(implicit codec: Codec): Future[String] =
    JSFsPromises.readFile(file.toString, codec.name).toFuture

  def readStdinAsync: Future[String] = JSIO.readStdinAsync

  def writeFile(path: Path, content: String)(implicit codec: Codec): Unit = JSFs
    .writeFileSync(path.toString, content, codec.name)

  def writeFileAsync(file: Path, data: String)(implicit
      codec: Codec,
  ): Future[Unit] = JSFsPromises.writeFile(file.toString, data, codec.name)
    .toFuture

  def cwd() = js.Dynamic.global.process.cwd().asInstanceOf[String]
}
