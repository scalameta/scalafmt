package org.scalafmt.benchmarks

import java.io.File
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.Warmup
import org.scalafmt.Scalafmt
import org.scalafmt.ScalafmtStyle
import org.scalafmt.util.FileOps

import scala.meta.Source
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences.FormattingPreferences
import scalariform.formatter.preferences.IndentSpaces

/**
  * Formats filename at [[path]] with scalafmt and scalariform.
  *
  * To run benchmark:
  *
  * > benchmarks/jmh:run -i 10 -wi 10 -f1 -t1 org.scalafmt.*
  */
@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 5, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class FormatBenchmark(path: String*) {
  val scalariformPreferences =
    FormattingPreferences().setPreference(IndentSpaces, 3)
  val classLoader = getClass.getClassLoader
  var code: String = _

  @Setup
  def setup(): Unit = {
    code = FileOps.readFile(getPath.getAbsolutePath)
  }

  def getPath: File = {
    val filename = FileOps.getFile(Seq("src", "resources") ++ path: _*)
    // jmh runs from benchmarks directory while tests run from from root.
    // Can't bother to find more generic solution
    if (filename.isFile) filename
    else FileOps.getFile(Seq("benchmarks", "src", "resources") ++ path: _*)
  }

  def scalametaParser(): Unit = {
    import scala.meta._
    code.parse[Source]
  }

  @Benchmark
  def scalafmt(): String = {
    Scalafmt.format(code).get
  }

  // No need to run same benchmark again and again.
//  @Benchmark
  def scalariform(): String = {
    ScalaFormatter.format(code, scalariformPreferences)
  }
}

object run {

  abstract class ScalaJsFile(filename: String)
      extends FormatBenchmark("scala-js", filename)

//  class OptimizerCore extends ScalaJsFile("OptimizerCore.scala")

  class GenJsCode extends ScalaJsFile("GenJSCode.scala")

//  class ScalaJSClassEmitter
//      extends ScalaJsFile("ScalaJSClassEmitter.scala")
//
//  class JavaLangString extends ScalaJsFile("JavaLangString.scala")
}
