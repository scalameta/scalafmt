package org.scalafmt.benchmarks

import java.nio.file.FileSystems
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations.Measurement
import org.openjdk.jmh.annotations.Mode
import org.openjdk.jmh.annotations.OutputTimeUnit
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.Setup
import org.openjdk.jmh.annotations.Warmup
import org.scalafmt.ScalaFmt
import org.scalafmt.ScalaStyle

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
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class FormatBenchmark(path: String*) {

  val scalariformPreferences =
    FormattingPreferences().setPreference(IndentSpaces, 3)
  val classLoader = getClass.getClassLoader
  val separator = FileSystems.getDefault.getSeparator
  val filename = path.mkString(separator)
  var code: String = _

  @Setup
  def setup(): Unit = {
    code = new String(Files.readAllBytes(getPath))
  }

  def getPath: Path = {
    val path = Paths.get("src", "resources", filename)
    // jmh runs from benchmarks directory while tests run from from root.
    // Can't bother to find more generic solution
    if (Files.isRegularFile(path)) path
    else Paths.get("benchmarks", "src", "resources", filename)
  }

  @Benchmark
  def scalametaParser(): Unit = {
    import scala.meta._
    code.parse[Source]
  }

  @Benchmark
  def scalafmt(): String = {
    ScalaFmt.format_![Source](code,
      ScalaStyle.Standard)(scala.meta.parsers.parseSource)
  }

  @Benchmark
  def scalariform(): String = {
    ScalaFormatter.format(code, scalariformPreferences)
  }
}

object run {

  // Scala-js benchmarks
  abstract class ScalaJsBenchmark(filename: String)
    extends FormatBenchmark("scala-js", filename)

  class Basic extends FormatBenchmark("scalafmt", "Basic.scala")

  class Utils extends ScalaJsBenchmark("Utils.scala")

  class Division extends ScalaJsBenchmark("Division.scala")

  class JsDependency extends ScalaJsBenchmark("JSDependency.scala")

  class SourceMapWriter extends ScalaJsBenchmark("SourceMapWriter.scala")

  class BaseLinker extends ScalaJsBenchmark("BaseLinker.scala")

  class Semantics extends ScalaJsBenchmark("Semantics.scala")

  // Scalafmt can't format these, yet.
  //  class TypeKinds extends ScalaJsBenchmark("TypeKinds.scala")
  //  class Analyzer extends ScalaJsBenchmark("Analyzer.scala")
  //  class Trees extends ScalaJsBenchmark("Trees.scala")
  //  class OptimizerCore extends ScalaJsBenchmark("OptimizerCore.scala")
  //  class GenJsCode extends ScalaJsBenchmark("GenJsCode.scala")
  //  class CopyOnWriteArrayList extends ScalaJsBenchmark("CopyOnWriteArrayList.scala")
}

