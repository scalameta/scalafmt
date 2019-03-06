package org.scalafmt.util

import java.io.File
import java.net.URL

import org.apache.commons.io.FileUtils
import org.rauschig.jarchivelib.ArchiverFactory

case class ScalaFile(filename: String, projectUrl: String, commit: String) {

  def rawUrl = {
    val raw = projectUrl.replace("github.com", "raw.githubusercontent.com")
    s"$raw/$commit$filename"
  }

  def read: String = {
    val toRead = new File(FileOps.getFile("target", "repos", repo), filename)
    FileOps.readFile(toRead)
  }

  def githubUrl = s"$projectUrl/blob/$commit$filename"
  def githubUrlAtLine(line: Int): String = s"$githubUrl#L$line"
  def userRepo = projectUrl.stripPrefix("https://github.com/")
  def repo = userRepo.split("/")(1)
  def user = userRepo.split("/")(0)

  override def toString: String = s"""ScalaFile(
                                     |    project: $user
                                     |    github: $githubUrl
                                     |    raw: $rawUrl
                                     |)""".stripMargin
}

object ScalaFile {
  private val tarballName = "repos"
  private val tarballNameWithExt = s"$tarballName.tar.gz"
  private val reposTarballUrl =
    s"https://github.com/scalameta/scalafmt/releases/download/v0.1.4/$tarballNameWithExt"

  def getAll: Seq[ScalaFile] = {
    val repos = FileOps.getFile("target", tarballName)
    if (!repos.isDirectory) createReposDir()

    val files = Option(repos.listFiles()).getOrElse {
      throw new IllegalStateException(
        s"""${repos.getAbsolutePath} is not a directory, run:
           |* wget $reposTarballUrl
           |* tar xvf $tarballNameWithExt
           |""".stripMargin
      )
    }

    Seq(files: _*).flatMap { repo =>
      val repoPrefix = repo.getPath
      val commit = FileOps.readFile(new File(repo, "COMMIT")).trim
      val url = FileOps.readFile(new File(repo, "URL")).trim
      FileOps
        .listFiles(repo)
        .withFilter(_.endsWith(".scala"))
        .withFilter(includeFile)
        .map { sourceFile =>
          val filename = sourceFile.stripPrefix(repoPrefix)
          ScalaFile(filename.trim, url, commit)
        }
    }
  }

  /** If needed, downloads the tarball containing sources from different projects and extracts these files. */
  private def createReposDir(): Unit = {
    val currentDir = new File(".")
    val localTarball = new File(currentDir, tarballNameWithExt)
    if (!FileOps.getFile(tarballNameWithExt).isFile) {
      downloadReposTar(destination = localTarball)
    }
    extractReposTar(localTarball, destination = currentDir)
  }

  private def downloadReposTar(destination: File): Unit = {
    val fileToDownload = new URL(reposTarballUrl)
    println(s"Downloading $reposTarballUrl...")
    FileUtils.copyURLToFile(fileToDownload, destination)
    println("Download finished.")
  }

  private def extractReposTar(tarball: File, destination: File): Unit = {
    val archiver = ArchiverFactory.createArchiver("tar", "gz")
    println(s"Extracting ${tarball.getAbsolutePath}...")
    archiver.extract(tarball, destination)
    println("Extracting finished.")
  }

  private def includeFile(filename: String): Boolean = {
    !Seq(
      // Computer generated
      "library/src/main/scala/scala/scalajs/js/Tuple.scala",
      // This fella seems to make the scalac parser hang (???)
      "target/repos/scala/test/files/neg/t5510.scala",
      // Unicode escapes in weird places
      "target/repos/scala/test/files/neg/t8015-ffb.scala",
      "target/repos/scala/test/files/pos/t389.scala",
      "target/repos/scala/test/files/run/literals.scala",
      "target/repos/scala/test/files/run/t3835.scala",
      // Scalac parser seems to accept this, though it blows up later
      "target/repos/scala/test/files/neg/t8266-invalid-interp.scala",
      "target/repos/scala/test/disabled/",
      "target/repos/scala/test/files/neg/",
      // trailing . after number
      "target/repos/scala/test/files/presentation/infix-completion/src/Snippet.scala",
      // Unicode escapes in weird places
      "target/repos/sbt/main/settings/src/main/scala/sbt/std/InputWrapper.scala",
      // uses a package called `macro`
      "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
      "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro",
      "target/repos/lila/modules/lobby/src/main/SocketHandler.scala"
    ).exists(filename.contains)
  }
}
