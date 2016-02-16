package org.scalafmt.benchmarks

import java.nio.file.FileSystem
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
import org.scalafmt.Standard

import scala.meta.Source
import scalariform.formatter.ScalaFormatter
import scalariform.formatter.preferences._

@org.openjdk.jmh.annotations.State(Scope.Benchmark)
@Warmup(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@Measurement(iterations = 10, time = 1, timeUnit = TimeUnit.SECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
abstract class FormatBenchmark(path: String*) {

  var code: String = _

  val scalafmtFormatter = new ScalaFmt(Standard)

  val scalariformPreferences =
    FormattingPreferences().setPreference(IndentSpaces, 3)

  val classLoader = getClass.getClassLoader

  val separator = FileSystems.getDefault.getSeparator
  val filename = path.mkString(separator)

  def getPath: Path = {
    val path = Paths.get("src", "resources", filename)
    // jmh runs from benchmarks directory while tests run from from root.
    // Can't bother to find more generic solution
    if (Files.isRegularFile(path)) path
    else Paths.get("benchmarks", "src", "resources", filename)
  }

  @Setup
  def setup(): Unit = {
    code = new String(Files.readAllBytes(getPath))
  }

  @Benchmark
  def scalafmt(): String = {
    scalafmtFormatter.format_![Source](code)(scala.meta.parsers.parseSource)
  }

  @Benchmark
  def scalariform(): String = {
    ScalaFormatter.format(code, scalariformPreferences)
  }
}

object run {
  class Basic extends FormatBenchmark("scalafmt", "Basic.scala")

  // Scala-js benchmarks
  abstract class ScalaJsBenchmark(filename: String)
    extends FormatBenchmark("scala-js", filename)

  class Utils extends ScalaJsBenchmark("Utils.scala")
  class Division extends ScalaJsBenchmark("Division.scala")
  class JsDependency extends ScalaJsBenchmark("JsDependency.scala")
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

