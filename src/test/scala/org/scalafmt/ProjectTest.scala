package org.scalafmt

import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.CopyOnWriteArrayList

import collection.JavaConversions._

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.meta._

/**
 * Mostly borrowed from
 * https://github.com/lihaoyi/fastparse/blob/0d67eca8f9264bfaff68e5cbb227045ceac4a15f/scalaparse/jvm/src/test/scala/scalaparse/ProjectTests.scala
 */
class ProjectTest {

  var parseFailures: java.util.List[ParseErr] = new CopyOnWriteArrayList
  var otherFailures: java.util.List[UnknownFailure] = new CopyOnWriteArrayList
  var successes: java.util.List[ParseSuccess] = new CopyOnWriteArrayList
  var skipped: java.util.List[String] = new CopyOnWriteArrayList

  val pathRoot = "target/repos/"
  def repoName(url: String): String = url.split("/").last

  case class ParseSuccess(filename: String, millis: Long)
  case class UnknownFailure(url: String, e: Throwable) {
    override def toString: String = s"$url $e"
  }
  case class ParseErr(url: String, e: ParseException) {
    def err: String = e.getMessage.replaceAll(" at .*", "")
    def lineNumber = e.pos.point.line
    def cols = s"cols:${e.pos.start.column}-${e.pos.end.column}"
    def content = s"cols:${e.pos.start.column}-${e.pos.end.column}"
    override def toString: String = s"$url#L${e.pos.start.line + 1} $cols"
  }

  def parse(filename: String): Future[Boolean] = Future {
    val code = new String(java.nio.file.Files.readAllBytes(java.nio.file.Paths.get(filename)))
    if (!ScalacParser.checkParseFails(code)) {
      print(".")
      val startTime = System.currentTimeMillis()
      val parsed = code.parse[Source]
      successes.add(ParseSuccess(filename, System.currentTimeMillis() - startTime))
    } else {
      skipped.add(filename)
    }
  }

  def checkRepo(url: String, filter: String => Boolean = _ => true) = {
    import sys.process._
    import FilesUtil._
    val name = repoName(url)
    val path = pathRoot + name
    println("CLONING?")
    if (!Files.exists(Paths.get("target", "repos", name))) {
      println("CLONING")
      val exit = List("git", "clone", url, "target/repos/" + name, "--depth", "1").!
    }
    val branch = Seq("git", s"--git-dir=$path/.git",
      "rev-parse", "HEAD").!!
    println("Checking project " + name)
    val files = listFiles(path)
      .withFilter(x => filter(x) && x.endsWith(".scala"))
      .map(filename => {
        val fileUrl = s"$url/blob/$branch${filename.stripPrefix(path)}"
        parse(filename).recover {
          case e: ParseException =>
            parseFailures.add(ParseErr(fileUrl, e))
          case e: scala.Exception =>
            otherFailures.add(UnknownFailure(fileUrl, e))
        }
      })
    files.foreach(Await.result(_, 1.minute))
    println()
  }

  def sbtIgnore: String => Boolean = x => !Seq(
    // Unicode escapes in weird places
    "target/repos/sbt/main/settings/src/main/scala/sbt/std/InputWrapper.scala",
    // uses a package called `macro`
    "target/repos/sbt/sbt/src/sbt-test/source-dependencies/inherited-macros",
    "target/repos/sbt/sbt/src/sbt-test/source-dependencies/macro"
  ).exists(x.startsWith)

  def scalaIgnore: String => Boolean = x => !Seq(
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
    "target/repos/scala/test/files/presentation/infix-completion/src/Snippet.scala"
  ).exists(x.startsWith)
  def header(msg: String) = s"\n\n## $msg\n"

  def bullet[T](msg: T) = s"* $msg"

  def run: Unit = {
    checkRepo("https://github.com/scala/scala", scalaIgnore)
    checkRepo("https://github.com/akka/akka")
    checkRepo("https://github.com/apache/spark")
    checkRepo("https://github.com/non/cats")
    checkRepo("https://github.com/lihaoyi/fastparse")
    checkRepo("https://github.com/scala-js/scala-js")
    checkRepo("https://github.com/scalaz/scalaz")
    checkRepo("https://github.com/milessabin/shapeless")
    checkRepo("https://github.com/akka/akka")
    checkRepo("https://github.com/lift/framework")
    checkRepo("https://github.com/playframework/playframework")
    checkRepo("https://github.com/PredictionIO/PredictionIO")
    checkRepo("https://github.com/apache/spark")
    checkRepo("https://github.com/sbt/sbt", sbtIgnore)
    checkRepo("https://github.com/non/cats")
    checkRepo("https://github.com/twitter/finagle")
    checkRepo("https://github.com/apache/kafka")
    checkRepo("https://github.com/scalanlp/breeze")
    checkRepo("https://github.com/non/spire")
    checkRepo("https://github.com/saddle/saddle")
    checkRepo("https://github.com/scala-ide/scala-ide")
    checkRepo("https://github.com/scalafx/scalafx")
    checkRepo("https://github.com/scalafx/scalafx-ensemble")
    checkRepo("https://github.com/takezoe/gitbucket")
    checkRepo("https://github.com/twitter/scalding")
    checkRepo("https://github.com/pocorall/scaloid")
    checkRepo("https://github.com/mesosphere/marathon")
    checkRepo("https://github.com/scalatra/scalatra")
    checkRepo("https://github.com/twitter/summingbird")
    checkRepo("https://github.com/slick/slick")
    checkRepo("https://github.com/ensime/ensime-server")
    checkRepo("https://github.com/GravityLabs/goose")
    checkRepo("https://github.com/ornicar/lila",
      x => !Seq(
        "target/repos/lila/modules/lobby/src/main/SocketHandler.scala"
      ).exists(x.startsWith)
    )
    checkRepo("https://github.com/precog/platform")
    checkRepo("https://github.com/twitter/util")
    checkRepo("https://github.com/scala/pickling")
    checkRepo("https://github.com/JetBrains/intellij-scala")
    for ((msg, parseErrors) <- parseFailures.groupBy(_.err)) {
      println(header(msg))
      parseErrors.sortBy(_.url).map(bullet).foreach(println)
    }
    println(header("scala.Exception"))
    otherFailures.toIterator.map(bullet).foreach(println)
    println(header("Summary:"))
    println(s"Unknown Failures: ${otherFailures.length}")
    println(s"Parse exceptions: ${parseFailures.length}")
    val avgTime = successes.map(_.millis).sum.toDouble / successes.length
    println(f"Successes: ${successes.length}, avg time to parse a single file $avgTime%2.2f ms")
    println(s"Skipped: ${skipped.length}")
  }
}
