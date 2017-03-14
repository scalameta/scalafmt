package org.scalafmt.bootstrap

import java.io.OutputStreamWriter
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, PrintStream}
import java.net.URLClassLoader

import coursier._
import org.scalafmt.Versions

import scala.collection.immutable.Nil
import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.util.{Failure, Success}
import scalaz.{\/, \/-}
import scalaz.concurrent.Task

class FetchError(errors: Seq[(Dependency, Seq[String])])
    extends Exception(errors.toString())

sealed abstract class ScalafmtBootstrap(val cli: ScalafmtCli) {

  def main(array: Seq[String]): Unit = {
    cli.main(array.to[Array])
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

object ScalafmtBootstrap {
  private val cliCache =
    mutable.Map.empty[String, Either[Throwable, ScalafmtBootstrap]]

  def fromVersion(version: String): Either[Throwable, ScalafmtBootstrap] =
    cliCache.getOrElseUpdate(version, fromVersionUncached(version))

  def fromVersionUncached(
      version: String): Either[Throwable, ScalafmtBootstrap] = {
    val start =
      Resolution(
        Set(
          Dependency(Module("com.geirsson", "scalafmt-cli_2.11"), version)
        )
      )

    val repositories =
      MavenRepository("https://repo1.maven.org/maven2") :: {
        // ivy2 local is only necessary when testing the sbt plugin on a locally
        // published version of scalafmt. See https://github.com/scalameta/scalafmt/issues/807
        // for a potential error caused by resolving fron ivy2 local (I can't reproduce the
        // error so this is a wild guess).
        if (sys.props.contains("scalafmt.scripted")) Cache.ivy2Local :: Nil
        else Nil
      }

    val logger = new TermDisplay(new OutputStreamWriter(System.err))
    logger.init(System.err.println("Downloading scalafmt artifacts..."))
    val fetch = Fetch.from(
      repositories,
      Cache.fetch(
        logger = Some(logger)
      )
    )
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
        case Success(cli) => Right(new ScalafmtBootstrap(cli) {})
        case Failure(e) => Left(e)
      }
    }
  }

  def main(args: Seq[String]): Unit = {
    fromVersion(Versions.nightly) match {
      case Right(cli) => cli.main(args)
      case Left(e) => throw e
    }
  }
}
