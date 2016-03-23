package org.scalafmt.cli

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import org.scalafmt.util.DiffAssertions
import org.scalafmt.util.FileOps
import org.scalatest.FunSuite

import scala.concurrent.duration.Duration
import scala.tools.nsc.classpath.FileUtils

class CliTest extends FunSuite with DiffAssertions {
  val unformatted = """
                      |object a    extends   App {
                      |println("hello world!")
                      |}
                    """.stripMargin
  val expected = """object a extends App {
                   |  println("hello world!")
                   |}
                 """.stripMargin
  test("cli parses args") {
    val expected = Cli.Config(Some(new File("foo")), inPlace = true)
    val args = Array("--file", "foo", "-i")
    val obtained = Cli.getConfig(args)
    assert(obtained.contains(expected))
  }

  test("scalafmt -i --file tmpFile") {
    val tmpFile = Files.createTempFile("prefix", ".scala")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace = Cli.Config(Some(tmpFile.toFile), inPlace = true)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, expected)
  }

  test("scalafmt -i ignores non-scala files") {
    val tmpFile = Files.createTempFile("prefix", "suffix")
    Files.write(tmpFile, unformatted.getBytes)
    val formatInPlace = Cli.Config(Some(tmpFile.toFile), inPlace = true)
    Cli.run(formatInPlace)
    val obtained = FileOps.readFile(tmpFile.toString)
    assertNoDiff(obtained, unformatted)
  }
}
