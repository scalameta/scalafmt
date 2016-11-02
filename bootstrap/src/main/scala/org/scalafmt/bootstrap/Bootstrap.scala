package org.scalafmt.bootstrap

import scala.language.reflectiveCalls

import scala.collection.immutable.Nil
import scala.collection.mutable
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scalaz.\/
import scalaz.\/-
import scalaz.concurrent.Task

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.File
import java.io.PrintStream
import java.net.URLClassLoader

import com.typesafe.config.ConfigFactory
import coursier._
import org.scalafmt.util.AbsoluteFile
import org.scalafmt.util.GitOps

class FetchError(errors: Seq[(Dependency, Seq[String])])
    extends Exception(errors.toString())

sealed abstract class Scalafmt(val cli: ScalafmtCli) {

  def main(array: Array[String]): Unit = {
    cli.main(array)
  }

  def format(code: String): String = {
    val baos = new ByteArrayOutputStream()
    val workingDirectory = new File("").getAbsolutePath
    val ctx = cli.main(
      Array("--stdin"),
      new ByteArrayInputStream(code.getBytes()),
      new PrintStream(baos),
      new PrintStream(new ByteArrayOutputStream()),
      workingDirectory
    )
    new String(baos.toByteArray)
  }
}

object DefinesVersion {
  def unapply(arg: AbsoluteFile): Option[String] =
    Scalafmt.getVersion(arg / ".scalafmt.conf")
}

object Scalafmt {
  private val cliCache = mutable.Map.empty[String, Either[Throwable, Scalafmt]]

  def getVersion(file: AbsoluteFile): Option[String] =
    Try {
      val config = ConfigFactory.parseFile(file.jfile)
      config.getString("version")
    }.toOption

  def fromGitOps(gitOps: GitOps): Either[Throwable, Scalafmt] = {
    val version = Seq(Some(AbsoluteFile.userDir),
                      gitOps.rootDir,
                      Some(AbsoluteFile.homeDir)).collectFirst {
      case Some(DefinesVersion(v)) => v
    }.getOrElse(org.scalafmt.Versions.stable)
    fromVersion(version)
  }

  def fromAuto: Either[Throwable, Scalafmt] = {
    fromGitOps(GitOps())
  }

  def fromVersion(version: String): Either[Throwable, Scalafmt] =
    cliCache.getOrElseUpdate(version, fromVersionUncached(version))

  def fromVersionUncached(version: String): Either[Throwable, Scalafmt] = {
    val start =
      Resolution(
        Set(
          Dependency(Module("com.geirsson", "scalafmt-cli_2.11"), version)
        )
      )

    val repositories = Seq(
      Cache.ivy2Local,
      MavenRepository("https://repo1.maven.org/maven2")
    )

    val fetch = Fetch.from(repositories, Cache.fetch())
    val resolution = start.process.run(fetch).unsafePerformSync
    val errors: Seq[(Dependency, Seq[String])] = resolution.errors
    if (errors.nonEmpty) Left(new FetchError(errors))
    else {
      val localArtifacts: Seq[FileError \/ File] = Task
        .gatherUnordered(
          resolution.artifacts.map(Cache.file(_).run)
        )
        .unsafePerformSync
      val urls = localArtifacts.collect {
        case \/-(file) => file.toURI.toURL
      }
      val classLoader = new URLClassLoader(urls.toArray, null)
      val reflectiveDynamicAccess = new ReflectiveDynamicAccess(classLoader)
      val loadedClass =
        reflectiveDynamicAccess
          .createInstanceFor[ScalafmtCli]("org.scalafmt.cli.Cli$", Nil)
      loadedClass match {
        case Success(cli) => Right(new Scalafmt(cli) {})
        case Failure(e) => Left(e)
      }
    }
  }
}

object Bootstrap {
  def main(args: Array[String]): Unit = {
    Scalafmt.fromAuto match {
      case Right(cli) => cli.main(args)
      case Left(e) => throw e
    }
  }
}
